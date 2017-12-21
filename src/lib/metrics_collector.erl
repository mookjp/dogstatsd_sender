-module(metrics_collector).

%% API
-export([
  message_queue_len/1
]).

-spec message_queue_len(pid()) -> non_neg_integer().
message_queue_len(Pid) ->
  proplists:get_value(message_queue_len, erlang:process_info(Pid)).
