-module(dogstatsd_sender).

-export([
  register/1,
  register/2,
  unregister/1,
  send/1
]).

-spec register(metrics_manager:collector_param()) -> ok.
register(Params) ->
  metrics_manager:register(self(), Params#{ from => self() }).

-spec register(pid(), metrics_manager:collector_param()) -> ok.
register(Pid, Params) ->
  metrics_manager:register(Pid, Params#{ from => self() }).

-spec unregister(pid()) -> ok.
unregister(Pid) ->
  metrics_manager:unregister(self(), Pid).

-spec send(dogstatsd_client:dogstatsd_client_req()) -> ok.
send(Params) ->
  dogstatsd_client:send(Params).
