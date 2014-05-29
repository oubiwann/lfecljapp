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
(lfenode@cahwsx01)> lfecljapp.lfe:187:<0.44.0>:...
INFO: Starting clojure app with cmd "java -Dnode=...
```

At this point, you are in the shell, and before too long you should also see
a log message display showing the successful start of the Clojure node:

```
(lfenode@cahwsx01)> lfecljapp.lfe:118:<0.42.0>:...
INFO: Connection to java node established, pid: <6709.1.0>
```

## Using

Now that you've got both ends of the connection up, you can take it for a
spin with a ping command:

```cl
(lfenode@cahwsx01)> (lfecljapp:ping "clj-node@cahwsx01" "clj-mbox")
#(ping <0.32.0>)
```

The node name used in the example above was taken from the output when the
Clojure node was started up. In particular, look for the line beginning with
``INFO: Starting clojure app ...`` and the value associated with the ``-Dnode``
parameter. That's your destination node in this case.

To see the response from the Clojure node, you'll need to flush the shell:

```cl
(lfenode@cahwsx01)> (c:flush)
Shell got {pong,<6709.1.0>}
ok
```

Hurray! A successful response from the Clojure node!
