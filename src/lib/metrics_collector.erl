-module(metrics_collector).

%% API
-export([
  message_queue_len/1,

  %% For common tags
  pid/1,
  mfa/1,
  node/1
]).

-spec message_queue_len(pid()) -> non_neg_integer().
message_queue_len(Pid) ->
  proplists:get_value(message_queue_len, erlang:process_info(Pid)).

%% =============================================================================
%% For common tags
%% =============================================================================

-spec pid(pid()) -> string().
pid(Pid) ->
  erlang:pid_to_list(Pid).

-spec mfa(pid()) -> string().
mfa(Pid) ->
  {M, F, A} = proplists:get_value(current_function, erlang:process_info(Pid)),
  unicode:characters_to_list(io_lib:format("~p/~p/~p", [M, F, A]), utf8).

-spec node(pid()) -> string().
node(Pid) ->
  atom_to_list(erlang:node(Pid)).
