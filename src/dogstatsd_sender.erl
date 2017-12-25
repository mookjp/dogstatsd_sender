-module(dogstatsd_sender).

-export([
  register/1,
  register/2,
  unregister/0,
  unregister/1,
  send/1
]).

%% -------------------------------------------------------------------------
%% @doc Register caller pid as a registered process to monitor.
%% @end
%% -------------------------------------------------------------------------
-spec register(metrics_manager:collector_param()) -> ok.
register(Params) ->
  metrics_manager:register(self(), Params#{ from => self() }).

%% -------------------------------------------------------------------------
%% @doc Register specific pid as a registered process to monitor.
%% @end
%% -------------------------------------------------------------------------
-spec register(pid(), metrics_manager:collector_param()) -> ok.
register(Pid, Params) ->
  metrics_manager:register(Pid, Params#{ from => self() }).

%% -------------------------------------------------------------------------
%% @doc Unregister caller pid not to monitor anymore.
%% @end
%% -------------------------------------------------------------------------
-spec unregister() -> ok.
unregister() ->
  metrics_manager:unregister(self(), self()).

%% -------------------------------------------------------------------------
%% @doc Unregister specific pid not to monitor anymore.
%% @end
%% -------------------------------------------------------------------------
-spec unregister(pid()) -> ok.
unregister(Pid) ->
  metrics_manager:unregister(self(), Pid).

%% -------------------------------------------------------------------------
%% @doc Send metrics to dogstatsd.
%% @end
%% -------------------------------------------------------------------------
-spec send(dogstatsd_client:dogstatsd_client_req()) -> ok.
send(Params) ->
  dogstatsd_client:send(Params).
