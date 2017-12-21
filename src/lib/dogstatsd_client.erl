-module(dogstatsd_client).

-behaviour(gen_server).

%% API
-export([
  start_link/3,
  send/1
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

-type dogstatsd_client_req() :: #{
  metrics := term(),
  value := term(),
  type := g, %% g is only supported currently
  tags := [{ term(), term() }]
}.

-define(SERVER, ?MODULE).
-record(state, {
  address,
  port :: inet:port_number(),
  socket,
  metrics_prefix :: string()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Address :: string(), Port :: non_neg_integer(), Prefix :: atom) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Address, Port, Prefix) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Address, Port, Prefix], []).

-spec send(dogstatsd_client_req()) -> ok.
send(Params) ->
  gen_server:cast(?MODULE, {send, Params}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: [ Name :: atom() ]) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Address, Port, Prefix]) ->
  {ok, Socket} = gen_udp:open(0),
  {ok, #state{
    address = Address,
    port = Port,
    metrics_prefix = Prefix,
    socket = Socket
  }}.

-spec(handle_cast(Request :: {send, dogstatsd_client_req()} | connect, State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({send, #{ metrics := Metrics } = MetricsInfo}, State) ->
  MetricsWithPrefix = lists:concat([State#state.metrics_prefix, ".", Metrics]),
  DogstatsdReq = maps:update(metrics, MetricsWithPrefix, MetricsInfo),
  BinaryReq = list_to_binary(format(DogstatsdReq)),
  case (catch gen_udp:send(State#state.socket, State#state.address, State#state.port, BinaryReq)) of
    ok ->
      ok;
    _ ->
      gen_server:cast(?MODULE, connect)
  end,
  {noreply, State};
handle_cast(connect, State) ->
  case (catch gen_udp:open(0)) of
    {ok, Socket} ->
      NewState = State#state{socket = Socket},
      {noreply, NewState};
    _ ->
      gen_server:cast(?MODULE, connect),
      {noreply, State}
  end.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% no functionality callbacks
%%%===================================================================

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

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
  {noreply, State}.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format(#{ metrics := Metrics, value := Value, type := Type, tags := Tags }) ->
  FormatBase = format(#{ metrics => Metrics, value => Value, type => Type }),
  TagStr = format_tags_to_str(Tags),
  JoinedTags = string:join(TagStr, ","),
  case length(JoinedTags) of
    0 ->
      FormatBase;
    _ ->
      lists:concat([FormatBase, "|#", JoinedTags])
  end;
format(#{ metrics := Metrics, value := Value, type := Type }) ->
  lists:concat([Metrics, ":", Value, "|", Type]).

format_tags_to_str(Tags) when is_list(Tags) ->
  lists:foldr(fun(TagTuple, Acc) ->
                {K, V} = TagTuple,
                Formatted = lists:concat([K, ":", V]),
                case Acc of
                  [] ->
                    [Formatted];
                  Former ->
                    [Formatted | Former]
                end
             end, [], Tags).
