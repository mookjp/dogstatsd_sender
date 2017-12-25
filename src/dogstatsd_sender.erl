-module(dogstatsd_sender).

-export([
  register/1,
  register/2,
  register/3,
  unregister/1,
  unregister/2,
  send/1
]).

-spec register(metrics_manager:collector_param()) -> ok.
register(Params) ->
  metrics_manager:register(self(), self(), Params).

-spec register(pid(), metrics_manager:collector_param()) -> ok.
register(Pid, Params) ->
  metrics_manager:register(self(), Pid, Params).

-spec register(pid(), pid(), metrics_manager:collector_param()) -> ok.
register(From, Pid, Params) ->
  metrics_manager:register(From, Pid, Params).

-spec unregister(pid()) -> ok.
unregister(Pid) ->
  dogstatsd_sender:unregister(self(), Pid).

-spec unregister(pid(), pid()) -> ok.
unregister(From, Pid) ->
  metrics_manager:unregister(From, Pid).

-spec send(dogstatsd_client:dogstatsd_client_req()) -> ok.
send(Params) ->
  dogstatsd_client:send(Params).
