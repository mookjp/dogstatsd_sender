-module(metrics_manager).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  register/2,
  unregister/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export_type([
  collector_param/0
]).

-type collector_param() :: #{
  metrics_list := [ message_queue_len ], %% message_queue_len is only supported currently
  common_tag_list := [ pid | mfa | node ],
  tags := [{ term(), term() }],
  from := pid()
}.
-type target() :: { pid(), collector_param() }.
-type register_param() :: #{ from := pid(), target := target() }.
-type unregister_param() :: #{ from := pid(), target := pid() }.

-record(state, {
  targets :: [ target() ],
  timer_ref,
  interval_ms :: non_neg_integer()
}).

-define(SUPPORTED_METRICS, [
  message_queue_len,
  'garbage_collection.minor_gcs'
]).

-define(SUPPORTED_COMMON_TAGS, [
  pid,
  mfa,
  node,
  registered_name
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(IntervalMs :: non_neg_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(IntervalMs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [IntervalMs], []).

-spec register(pid(), collector_param()) -> ok.
register(Pid, #{ common_tag_list := CommonTagList, tags := Tags, from := From } = Params)
    when is_pid(Pid), is_pid(From), is_list(CommonTagList), is_list(Tags) ->
  case (catch validate_params(Params)) of
    true ->
      gen_server:cast(?MODULE, {register, #{ from => From, target => {Pid, Params} }});
    _ ->
      throw(invalid_params)
  end;
register(Pid, Params) ->
  WithEmptyCommonTags = case maps:is_key(common_tag_list, Params) of
                          true ->
                            Params;
                          false ->
                            Params#{ common_tag_list => [] }
                        end,
  WithEmptyTags = case maps:is_key(tags, WithEmptyCommonTags) of
    true ->
      WithEmptyCommonTags;
    false ->
      WithEmptyCommonTags#{ tags => [] }
  end,
  metrics_manager:register(Pid, WithEmptyTags).

-spec unregister(pid(), pid()) -> ok.
unregister(From, Pid) ->
  gen_server:cast(?MODULE, {unregister, #{ from => From, target => Pid }}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([IntervalMs]) ->
  TimerRef = erlang:send_after(IntervalMs, self(), collect),
  {ok, #state{ targets = [], timer_ref = TimerRef, interval_ms = IntervalMs }}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: {register, register_param()} | {unregister, unregister_param()} | {collect, target()} | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({register, #{ from := From, target := {Pid, Params} }}, State) ->
  UpdatedTargets = [ {Pid, Params} | State#state.targets ],
  From ! {dogstatsd_manager, self(), {registered, #{ pid => Pid, params => maps:remove(from, Params) }}},
  {noreply, State#state{ targets = UpdatedTargets }};
handle_cast({unregister, #{ from := From, target := Pid }}, State) ->
  UpdatedTargets = proplists:delete(Pid, State#state.targets),
  From ! {dogstatsd_manager, self(), {unregistered, #{ pid => Pid }}},
  {noreply, State#state{ targets = UpdatedTargets }};
handle_cast({collect, {Pid, #{ metrics_list := MetricsList, common_tag_list := CommonTagList, tags := Tags, from := From } = _Params}}, State) ->
  case is_process_alive(Pid) of
    true ->
      lists:foreach(fun(Metrics) ->
        Value = collect(Pid, Metrics),
        CommonTags = collect_tags(Pid, CommonTagList),
        MergedTags = merge_tags(Tags, CommonTags),
        send(Metrics, Value, MergedTags)
                    end, MetricsList);
    false ->
      gen_server:cast(?MODULE, {unregister, #{ from => From, target => Pid }})
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(collect, State) ->
  erlang:cancel_timer(State#state.timer_ref, [{async, true}]),
  lists:foreach(fun(Target) ->
                  gen_server:cast(?MODULE, {collect, Target})
                end, State#state.targets),
  NewTimerRef = erlang:send_after(State#state.interval_ms, self(), collect),
  {noreply, State#state{ timer_ref = NewTimerRef }};
handle_info(_Req, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec collect(pid(), atom()) -> term().
collect(Pid, Metrics) ->
  ReplacedMetrics = replace_metrics_prefix(Metrics),
  erlang:apply(metrics_collector, ReplacedMetrics, [Pid]).

collect_tags(Pid, CommonTagList) ->
  lists:map(fun(Label) ->
              TagValue = collect_tag(Pid, Label),
              {Label, TagValue}
            end, CommonTagList).

collect_tag(Pid, Label) ->
  erlang:apply(metrics_collector, Label, [Pid]).

merge_tags(Tags, CommonTags) ->
  lists:foldr(fun({Label, Value}, CurrentTags) ->
                case proplists:get_value(Label, Tags) of
                  undefined ->
                    [{Label, Value} | CurrentTags];
                  _ ->
                    CurrentTags
                end
              end, Tags, CommonTags).

-spec send(atom(), term(), [{ atom(), term() }]) -> ok.
send(Metrics, Value, Tags) ->
  dogstatsd_client:send(#{
    metrics => Metrics,
    value => Value,
    type => g,
    tags => Tags
  }).

validate_params(#{ metrics_list := MetricsList, common_tag_list := CommonTagList } = _Params) ->
  lists:all(fun(Metrics) ->
              lists:any(fun(Supported) ->
                          Metrics =:= Supported
                        end, ?SUPPORTED_METRICS)
            end, MetricsList)
  andalso
    lists:all(fun(CommonTag) ->
      lists:any(fun(Supported) ->
        CommonTag =:= Supported
                end, ?SUPPORTED_COMMON_TAGS)
              end, CommonTagList).


%% -----------------------------------------------------------------------------
%% @doc This function replace metrics name to it without prefix; currently,
%% 'garbage_collection' only, dogstatsd_sender supports as the function to get
%% metrics is called by apply.
%% @end
%% -----------------------------------------------------------------------------
replace_metrics_prefix(Metrics) ->
  list_to_atom(re:replace(atom_to_list(Metrics), "garbage_collection\.", "", [{return, list}])).
