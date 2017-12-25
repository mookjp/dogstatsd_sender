

# Module dogstatsd_sender #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Register caller pid as a registered process to monitor.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td>Register specific pid as a registered process to monitor.</td></tr><tr><td valign="top"><a href="#send-1">send/1</a></td><td>Send metrics to dogstatsd.</td></tr><tr><td valign="top"><a href="#unregister-0">unregister/0</a></td><td>Unregister caller pid not to monitor anymore.</td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td>Unregister specific pid not to monitor anymore.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Params::<a href="metrics_manager.md#type-collector_param">metrics_manager:collector_param()</a>) -&gt; ok
</code></pre>
<br />

Register caller pid as a registered process to monitor.

<a name="register-2"></a>

### register/2 ###

<pre><code>
register(Pid::pid(), Params::<a href="metrics_manager.md#type-collector_param">metrics_manager:collector_param()</a>) -&gt; ok
</code></pre>
<br />

Register specific pid as a registered process to monitor.

<a name="send-1"></a>

### send/1 ###

<pre><code>
send(Params::<a href="dogstatsd_client.md#type-dogstatsd_client_req">dogstatsd_client:dogstatsd_client_req()</a>) -&gt; ok
</code></pre>
<br />

Send metrics to dogstatsd.

<a name="unregister-0"></a>

### unregister/0 ###

<pre><code>
unregister() -&gt; ok
</code></pre>
<br />

Unregister caller pid not to monitor anymore.

<a name="unregister-1"></a>

### unregister/1 ###

<pre><code>
unregister(Pid::pid()) -&gt; ok
</code></pre>
<br />

Unregister specific pid not to monitor anymore.

