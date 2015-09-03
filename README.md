# lfecljapp

<a href="resources/images/LispFlavoredErlangClojure-medium-square.png">
<img src="resources/images/LispFlavoredErlangClojure-small-square.png" />
</a>

*LFE-Clojure (JVM) Interop using JInterface*

## Introduction

This project is a port of Maxim Molchanov's example Erlang + Clojure interop
project (via JInterface). Only minor changes were made to the Clojure code.
The Erlang code was completely replaced with LFE.

This project demonstrates how one can:

1. Create a Clojure project which utilizes Erlang JInterface to communicate
   with LFE nodes,
1. Start a supervised Clojure node in LFE,
1. Send a message to Clojure nodes from LFE, and
1. Receive responses from the Clojure nodes in LFE.


### Dependencies

#### LFE Side of the House

* Erlang
* ``lfetool`` + ``rebar``

#### Clojure Side of the House

* Java
* ``lein``

## Building

To get started, build the Clojure Uberjar for ``cljnode`` and compile the
Erlang source files:

```bash
$ make compile
```

## Running

Once everything has compiled successfully, you can start an Erlang shell and
then bring the app up:

```bash
$ make repl
```

```lfe
(lfenode@cahwsx01)> (lfeclj-app:start)
ok
```

Or, you can just use the ``dev`` make target:

```bash
$ make dev
```

Once the app has started, you will see output like the following (elided):

```
(lfenode@cahwsx01)>
14:03:52.849 [info] Application lager started on node lfenode@cahwsx01
14:03:52.855 [info] Starting clojure app with cmd: "java ..."
```

At this point, you are in the shell, and before too long you should also see
a log message display showing the successful start of the Clojure node:

```
14:03:52.856 [info] Application lfecljapp started on node lfenode@cahwsx01
14:03:55.898 [info] Connection to java node established, pid: <11113.1.0>
```

## Using

Now that you've got both ends of the connection up, you can take it for a
spin with a ping command:

```cl
(lfenode@cahwsx01)> (lfecljapp:ping "clj-node@cahwsx01" "clj-mbox")
#(ping <0.83.0>)
```

The node name used in the example above was taken from the output when the
Clojure node was started up. In particular, look for the line beginning with
``INFO: Starting clojure app ...`` and the value associated with the ``-Dnode``
parameter. That's your destination node in this case.

To see the response from the Clojure node, you'll need to flush the shell:

```cl
(lfenode@cahwsx01)> (c:flush)
Shell got {pong,<11113.1.0>}
ok
```

Hurray! A successful response from the Clojure node!

## Fun for the Future

Here are some things I'd like to play with in this project:

* Setting up some example long-running computations in Clojure.
* Spawn multiple nodes and distribute computations across an LFE cluster.
* Use ``lfecljapp`` as a proxy to a Storm cluster, and explores ways in which
  it might be useful to interact with Storm from LFE.
