-module(estatsd_couch).

-export([start/0]).

start() ->
  loop().

loop() ->
  FlushIntervalMs = list_to_integer(couch_config:get("estatsd", "flush_interval", 10000)),
  GraphiteHost = couch_config:get("estatsd", "graphite_host", "localhost"),
  GraphitePort = list_to_integer(couch_config:get("estatsd", "graphite_port", 2003)),
  VmMetrics = list_to_atom(couch_config:get("estatsd", "vm_metrics", true)),
  VmName = couch_config:get("estatsd", "vm_name", node()),
  {ok, Pid} = estatsd_sup:start_link(FlushIntervalMs, GraphiteHost, GraphitePort, VmMetrics, VmName),

  couch_config:register(fun("estatsd") -> 
    exit(config_change, Pid),
    loop()
  end),
  {ok, Pid}.
