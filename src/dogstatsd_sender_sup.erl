-module(dogstatsd_sender_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  child_spec/2
]).

%% Supervisor callbacks
-export([
  init/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link(?MODULE, []).

child_spec(Id, MFA) ->
  #{
    id => Id,
    start => MFA,
    restart => permanent,
    shutdown => brutal_kill,
    type => worker
  }.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
  {ok, {#{strategy => one_for_one, intensity => 1000000, period => 1}, []}}.
