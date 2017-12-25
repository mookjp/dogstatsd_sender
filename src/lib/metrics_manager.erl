-module(metrics_manager).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  register/2,
  unregister/1
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
  tags := [{ term(), term() }]
}.
-type target() :: { pid(), collector_param() }.

-record(state, {
  targets :: [ target() ],
  timer_ref,
  interval_ms :: non_neg_integer()
}).

-define(SUPPORTED_METRICS, [
  message_queue_len
]).

-define(SUPPORTED_COMMON_TAGS, [
  pid,
  mfa,
  node
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(IntervalMs :: non_neg_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(IntervalMs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [IntervalMs], []).

-spec register(pid(), collector_param()) -> ok.
register(Pid, #{ common_tag_list := CommonTagList, tags := Tags } = Params)
    when is_pid(Pid), is_list(CommonTagList), is_list(Tags) ->
  case (catch validate_params(Params)) of
    true ->
      gen_server:cast(?MODULE, {register, {Pid, Params}});
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

unregister(Pid) ->
  gen_server:cast(?MODULE, {unregister, Pid}).


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

-spec(handle_cast(Request :: {register, target()} | {unregister, pid()} | {collect, target()} | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({register, {Pid, Params}}, State) ->
  UpdatedTargets = [ {Pid, Params} | State#state.targets ],
  {noreply, State#state{ targets = UpdatedTargets }};
handle_cast({unregister, Pid}, State) ->
  UpdatedTargets = proplists:delete(Pid, State#state.targets),
  {noreply, State#state{ targets = UpdatedTargets }};
handle_cast({collect, {Pid, #{ metrics_list := MetricsList, common_tag_list := CommonTagList, tags := Tags } = _Params}}, State) ->
  lists:foreach(fun(Metrics) ->
                  Value = collect(Pid, Metrics),
                  CommonTags = collect_tags(Pid, CommonTagList),
                  MergedTags = merge_tags(Tags, CommonTags),
                  send(Metrics, Value, MergedTags)
                end, MetricsList),
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
  erlang:apply(metrics_collector, Metrics, [Pid]).

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
