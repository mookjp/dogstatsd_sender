dogstatsd_sender
================================================================================

A simple client to dogstatsd plus periodical metrics collector.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
## Contents

- [Setup](#setup)
  - [Configuration parameters](#configuration-parameters)
- [Usage](#usage)
  - [Send your own metrics](#send-your-own-metrics)
  - [Register process to collect basic metrics; experimental function](#register-process-to-collect-basic-metrics-experimental-function)
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
dogstatsd_sender:send(#{ metrics => "my_metrics.point", value => 1, tags => [{label, sometihng}]}).
```

### Register process to collect basic metrics; experimental function

dogstatsd_sender supports to send some basic metrics via `dogstatsd_sernder:register/1`.


You give `metrics_list` key in your parameter map then dogstatsd_sender periodically collect the metrics.
The interval to collect metrics is configured by `metrics_interval_ms` in application environments.

```erlang
dogstatsd_sender:register(#{ metrics_list => [message_queue_len], tags => [{label, something}]}).
```

This is experimental function for now. Supported metrics in only:

* `message_queue_len`

## Modules

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender.md" class="module">dogstatsd_sender</a></td></tr>
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender_app.md" class="module">dogstatsd_sender_app</a></td></tr>
<tr><td><a href="http://github.com/mookjp/dogstatsd_sender/blob/master/doc/dogstatsd_sender_sup.md" class="module">dogstatsd_sender_sup</a></td></tr></table>

