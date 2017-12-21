%%%-------------------------------------------------------------------
%% @doc dogstatsd_sender public API
%% @end
%%%-------------------------------------------------------------------

-module(dogstatsd_sender_app).

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

-define(METRICS_INTERVAL_MS, application:get_env(dogstatsd_sender, metrics_interval_ms, 10000)).
-define(DOGSTATSD_ADDRESS, application:get_env(dogstatsd_sender, dogstatsd_address, "0.0.0.0")).
-define(DOGSTATSD_PORT, application:get_env(dogstatsd_sender, dogstatsd_port, 8125)).
-define(METRICS_PREFIX_BASE, application:get_env(dogstatsd_sender, metrics_prefix_base, "dogstatsd_sender")).


%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, Sup} = dogstatsd_sender_sup:start_link(),
  MetricsManagerSpec = dogstatsd_sender_sup:child_spec(metrics_manager,
    {metrics_manager, start_link, [?METRICS_INTERVAL_MS]}),
  DogstatsdClientSpec = dogstatsd_sender_sup:child_spec(dogstatsd_client,
    {dogstatsd_client, start_link, [?DOGSTATSD_ADDRESS, ?DOGSTATSD_PORT, ?METRICS_PREFIX_BASE]}),
  {ok, _MetricsManager} = supervisor:start_child(Sup, MetricsManagerSpec),
  {ok, _DogstatsdClient} = supervisor:start_child(Sup, DogstatsdClientSpec),
  {ok, Sup}.

stop(_State) ->
    ok.
