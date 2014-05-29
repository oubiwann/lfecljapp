# lfecljapp

*LFE-Clojure (JVM) Interop using JInterface.*

## Caveat

This is a work in progress. This project is being ported from its
predecessor, an Erlang-Clojure app.

## Building

To get started, build the Clojure Uberjar for ``cljnode`` and compile the
Erlang source files:

```bash
$ make compile
```

## Running

Once everything has compile successfully, you can start an Erlang shell and
then bring the app up:

```bash
$ make repl
```

```lfe
(lfenode@cahwsx01)> (lfecljapp-util:start)
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
(lfenode@cahwsx01)> (lfecljapp-util:ping 'clj_mbox 'clj_node@cahwsx01 (self))
#(ping <0.32.0>)
```

To see the response from the Clojure node, you'll need to flush the shell:

```cl
(lfenode@cahwsx01)> (c:flush)
Shell got {pong,<6709.1.0>}
ok
```

Hurray! A successful response from the Clojure node!
