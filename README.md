dogstatsd_sender
================================================================================

<img src="https://circleci.com/gh/mookjp/dogstatsd_sender/tree/master.svg?style=shield&circle-token=de845802556fbe80fdd67b01efdaaf238478d500">

A simple client to [dogstatsd](https://docs.datadoghq.com/guides/dogstatsd/); agent of [Datadog](https://www.datadoghq.com/), plus periodical metrics collector.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
## Contents

- [Setup](#setup)
  - [Configuration parameters](#configuration-parameters)
- [Usage](#usage)
  - [Send your own metrics](#send-your-own-metrics)
  - [Register process to collect basic metrics; experimental function](#register-process-to-collect-basic-metrics-experimental-function)
    - [Unregister](#unregister)
- [Modules](#modules)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Setup

Add this module as a dependency to your rebar.config if you use rebar3
and also add this module as an application to your .src file.

```
{deps, [
  {dogstatsd_sender, ".*", {git, "https://github.com/mookjp/dogstatsd_sender.git", {branch, "master"}}}
]}.
```

### Configuration parameters

```
[
  %% Address of dogstatsd
  {dogstatsd_address, "0.0.0.0"},
  %% Port number of dogstatsd
  {dogstatsd_port, 8125},
  %% Prefix of metrics
  %% e.g. #{ metrics => "my_metrics" } is sended as "dogstatsd_sender.my_metrics"
  {metrics_prefix_base, "dogstatsd_sender"},
  %% Interval to send metrics when you register your pid via dogstatsd_sender:register/2
  {metrics_interval_ms, 10000
]
```

## Usage

### Send your own metrics

```erlang
dogstatsd_sender:send(#{
  metrics => "my_metrics.point",
  type => g,
  value => 1,
  tags => [{label, sometihng}]
}).
```

Parameters for `dogstatsd_sender:send/1` supports [Dogststsd's metrics format](https://docs.datadoghq.com/guides/dogstatsd/#datagram-format).

* `metrics`
    * `metric.name` on [Dogstatsd docs](https://docs.datadoghq.com/guides/dogstatsd/#datagram-format)
* `type`
    * `g`, `c`, `ms`, `h`, `s` as atom
* `tags`
    * proplists with tag label as atom and tag value as atom also


By sending metrics with above example, it will send this to dogstatsd:

```
dogstatsd_sender.my_metrics.point:1|g|#label:something
```

Notice that dogstatsd_sender has configuration for the prefix of metrics name. Default prefix is `dogstatsd_sender` and it added as a prefix.
You can [change this prefix by your application configuration](#configuration-parameters).

### Register process to collect basic metrics; experimental function

dogstatsd_sender supports to send some basic metrics via `dogstatsd_sernder:register/1`.


You give `metrics_list` key in your parameter map then dogstatsd_sender periodically collect the metrics.
`common_tag_list` key can be set to send metrics with common process info; `pid`, `mfa`, `node`.


The interval to collect metrics is configured by `metrics_interval_ms` in application environments.


For example, you can set up metrics collector of dogstatsd_sender. Once you register a pid to it, it will automatically send metrics with tags:

```erlang
dogstatsd_sender:register(Pid, #{
  metrics_list => [message_queue_len],
  common_tag_list => [ pid, mfa, node ],
  tags => [{label, something}]
}),

receive 
  {dogstatsd_manager, _MngPid, {registered, #{ pid := _Pid, params := _RegisteredParams }}} ->
    ok
end.
```

`dogstatsd_sender:register` is async. You will get a reply message with above format.


This is experimental function for now. Supported metrics in only:

* `message_queue_len`


If you set common_tag_list, it will add tags supported automatically. Supported tags are below:

* `pid`
    * pid() of Erlang. e.g. `<0.100.0>`
* `mfa`
    * MFA of Erlang. e.g. `erl_eval/do_apply/6`
* `node`
    * node name. e.g. `node_name@host_name`


Above example will send metrics in this format to dogstatsd:

```
dogstatsd_sender.message_queue_len:10|g|#pid:<0.100.0>,mfa:erl_eval/do_apply/6,node:nodename@hostname,label:something
```

#### Unregister

To unregister the pid you registered, call this:

```erlang
dogstatsd_sender:unregister(Pid),

receive 
  {dogstatsd_manager, _MngPid, {unregistered, #{ pid := _Pid }}} ->
    ok
end.
```

`dogstatsd_sender:register` is also async. You will get a reply message with above format.

## Modules

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender.md" class="module">dogstatsd_sender</a></td></tr>
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender_app.md" class="module">dogstatsd_sender_app</a></td></tr>
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender_sup.md" class="module">dogstatsd_sender_sup</a></td></tr></table>

