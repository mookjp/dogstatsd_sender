-module(dogstatsd_sender).

-export([
  register/2,
  send/1
]).

-spec register(pid(), metrics_manager:collector_param()) -> ok.
register(Pid, Params) ->
  metrics_manager:register(Pid, Params).

-spec send(metrics_manager:collector_param()) -> ok.
send(Params) ->
  dogstatsd_client:send(Params).
