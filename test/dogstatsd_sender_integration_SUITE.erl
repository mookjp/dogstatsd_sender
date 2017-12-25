-module(dogstatsd_sender_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
  all/0,

  init_per_suite/1,
  end_per_suite/1,

  init_per_testcase/2,
  end_per_testcase/2,

  should_send_metrics_when_params_are_valid/1,
  should_send_metrics_without_tags_and_common_tags/1,
  should_throw_metrics_list_validation_exception/1,
  should_throw_common_tag_list_validation_exception/1
]).

all() -> [
  should_send_metrics_when_params_are_valid,
  should_send_metrics_without_tags_and_common_tags,
  should_throw_metrics_list_validation_exception,
  should_throw_common_tag_list_validation_exception
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

should_send_metrics_when_params_are_valid(_Config) ->
  Params = #{
    metrics_list => [
      message_queue_len
    ],
    common_tag_list => [
      pid,
      mfa
    ],
    tags => [
      {name, test},
      {type, test}
    ]
  },
  dogstatsd_sender:register(self(), Params),
  receive
    Msg ->
      ct:pal("msg: ~p", [Msg]),
      {udp, _Port, _Address, _Portnum, Packet} = Msg,
      {match, _} = re:run(Packet, "dogstatsd_sender\.message_queue_len:0|g|#pid:.+\,mfa:dogstatsd_sender_integration_SUITE/should_send_metrics_when_params_are_valid/1,name:test,type:test")
  after 1000 ->
    ct:fail("could not get any msg.")
  end,
  {comment, "it sends metrics to dogstatsd."}.

should_send_metrics_without_tags_and_common_tags(_Config) ->
  Params = #{
    metrics_list => [
      message_queue_len
    ]
  },
  dogstatsd_sender:register(self(), Params),
  receive
    Msg ->
      ct:pal("msg: ~p", [Msg]),
      {udp, _Port, _Address, _Portnum, Packet} = Msg,
      <<"dogstatsd_sender.message_queue_len:0|g">> = Packet
  after 1000 ->
    ct:fail("could not get any msg.")
  end,
  {comment, "it sends metrics to dogstatsd without tags and common tags."}.

should_throw_metrics_list_validation_exception(_Config) ->
  Params = #{
    metrics_list => [
      not_supported
    ]
  },
  case (catch dogstatsd_sender:register(self(), Params)) of
    invalid_params ->
      ok;
    _ ->
      ct:fail(unexpected)
  end,
  NoMetListParam = #{},
  case (catch dogstatsd_sender:register(self(), NoMetListParam)) of
    invalid_params ->
      ok;
    _ ->
      ct:fail(unexpected)
  end,
  {comment, "It throws exception if invalid metrics names are given."}.

should_throw_common_tag_list_validation_exception(_Config) ->
  Params = #{
    common_tag_list => [
      not_supported
    ]
  },
  case (catch dogstatsd_sender:register(self(), Params)) of
    invalid_params ->
      ok;
    _ ->
      ct:fail(unexpected)
  end,
  NoMetListParam = #{},
  case (catch dogstatsd_sender:register(self(), NoMetListParam)) of
    invalid_params ->
      ok;
    _ ->
      ct:fail(unexpected)
  end,
  {comment, "It throws exception if invalid common tags are given."}.
