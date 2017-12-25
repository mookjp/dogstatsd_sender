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
  should_throw_common_tag_list_validation_exception/1,
  should_not_send_metrics_after_unregister/1,
  should_delete_target_after_pid_died/1,

  receiver/0 %% For spawn
]).

all() -> [
  should_send_metrics_when_params_are_valid,
  should_send_metrics_without_tags_and_common_tags,
  should_throw_metrics_list_validation_exception,
  should_throw_common_tag_list_validation_exception,
  should_not_send_metrics_after_unregister,
  should_delete_target_after_pid_died
].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(should_delete_target_after_pid_died, Config) ->
  application:set_env(dogstatsd_sender, metrics_interval_ms, 1000),
  init_common(Config);
init_per_testcase(_, Config) ->
  application:set_env(dogstatsd_sender, metrics_interval_ms, 100),
  init_common(Config).

init_common(Config) ->
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
    {dogstatsd_manager, _MngPid, {registered, #{ pid := Self, params := Params }}} when Self =:= self() ->
      receive Msg ->
        ct:pal("msg: ~p", [Msg]),
        {udp, _Port, _Address, _Portnum, Packet} = Msg,
        {match, _} = re:run(Packet, "dogstatsd_sender\.message_queue_len:0|g|#pid:.+\,mfa:dogstatsd_sender_integration_SUITE/should_send_metrics_when_params_are_valid/1,name:test,type:test")
      after 1000 ->
        ct:fail("could not get any msg.")
      end
  after 1000 ->
    ct:fail("could not get registered msg.")
  end,
  {comment, "it sends metrics to dogstatsd."}.

should_send_metrics_without_tags_and_common_tags(_Config) ->
  Params = #{
    metrics_list => [
      message_queue_len
    ]
  },
  dogstatsd_sender:register(self(), Params),
  ExpectedParams = Params#{ tags => [], common_tag_list => [] },
  receive
    {dogstatsd_manager, _MngPid, {registered, #{ pid := Self, params := ExpectedParams }}} when Self =:= self() ->
      receive
        Msg ->
          ct:pal("msg: ~p", [Msg]),
          {udp, _Port, _Address, _Portnum, Packet} = Msg,
          <<"dogstatsd_sender.message_queue_len:0|g">> = Packet
      after 1000 ->
        ct:fail("could not get any msg.")
      end
  after 1000 ->
    ct:fail("could not get registered msg.")
  end,
  {comment, "it sends metrics to dogstatsd without tags and common tags."}.

should_not_send_metrics_after_unregister(_Config) ->
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
    {dogstatsd_manager, _MngPid, {registered, #{ pid := Self, params := Params }}} when Self =:= self() ->
      receive Msg ->
        ct:pal("msg: ~p", [Msg]),
        {udp, _Port, _Address, _Portnum, Packet} = Msg,
        {match, _} = re:run(Packet, "dogstatsd_sender\.message_queue_len:0|g|#pid:.+\,mfa:dogstatsd_sender_integration_SUITE/should_send_metrics_when_params_are_valid/1,name:test,type:test"),
        dogstatsd_sender:unregister(self()),
        receive
          {dogstatsd_manager, _MngPid, {unregistered, #{ pid := Self }}} when Self =:= self() ->
            receive
              Unexpected ->
                ct:pal("unexpected msg after unregister: ~p", [Unexpected]),
                ct:fail(unexpected_msg_after_unregister)
            after 1000 ->
              ok
            end
        after 1000 ->
          ct:fail("could not get unregistered msg.")
        end
      after 1000 ->
        ct:fail("could not get any msg.")
      end
  after 1000 ->
    ct:fail("could not get registered msg.")
  end,
  {comment, "it unregisters not to send metrics to datadog."}.

should_delete_target_after_pid_died(_Config) ->
  ct:pal("~p", [process_info(self())]),
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
  Pid = spawn(?MODULE, receiver, []),
  dogstatsd_sender:register(Pid, Params),
  receive
    {dogstatsd_manager, _MngPid, {registered, #{ pid := Pid, params := Params }}} ->
      receive Msg ->
        ct:pal("msg: ~p", [Msg]),
        {udp, _Port, _Address, _Portnum, Packet} = Msg,
        {match, _} = re:run(Packet, "dogstatsd_sender\.message_queue_len:0|g|#pid:.+\,mfa:dogstatsd_sender_integration_SUITE/receiver/0,name:test,type:test"),
        exit(Pid, kill),
        ct:sleep(1000),
        ct:pal("exited: ~p ~p", [Pid, is_process_alive(Pid)]),
        receive
          {dogstatsd_manager, _MngPid, {unregistered, #{ pid := Pid }}} ->
            receive
              Unexpected ->
                ct:pal("unexpected msg after unregister: ~p", [Unexpected]),
                ct:fail(unexpected_msg_after_unregister)
            after 2000 ->
              ok
            end
        after 2000 ->
          ct:fail("could not get unregistered msg.")
        end
      after 2000 ->
        ct:fail("could not get any msg.")
      end
  after 1000 ->
    ct:fail("could not get registered msg.")
  end,
  {comment, "it unregisters after target died."}.

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

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

receiver() ->
  receive Msg -> ct:pal("receiver msg: ~p", [Msg]) end.
