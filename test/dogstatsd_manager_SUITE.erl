-module(dogstatsd_manager_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
  all/0,

  init_per_suite/1,
  end_per_suite/1,

  init_per_testcase/2,
  end_per_testcase/2,

  should_send_counter_type/1
]).

all() -> [
  should_send_counter_type
].

init_per_suite(Config) ->
  application:set_env(dogstatsd_sender, metrics_interval_ms, 100),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
  {ok, Port} = inet:port(Socket),
  application:set_env(dogstatsd_sender, dogstatsd_port, Port),
  application:ensure_all_started(dogstatsd_sender),
  [{socket, Socket} | Config].

end_per_testcase(_, Config) ->
  application:stop(dogstatsd_sender),
  gen_udp:close(?config(socket, Config)),
  Config.

should_send_counter_type(_Config) ->
  Params = #{
    metrics => "my_counter",
    value => 10,
    type => c,
    tags => [
      {name, test},
      {type, test}
    ]
  },
  dogstatsd_client:send(Params),
  receive
    Msg ->
      ct:pal("msg: ~p", [Msg]),
      {udp, _Port, _Address, _Portnum, Packet} = Msg,
      <<"dogstatsd_sender.my_counter:10|c|#name:test,type:test">> = Packet
  after 1000 ->
    ct:fail("could not get any msg.")
  end,
  {comment, "it sends metrics to dogstatsd."}.
