# erlang-clojure-node

To get started, build the Clojure Uberjar for ``cljnode`` and compile the
Erlang source files:

## Building

```bash
$ make compile
```

## Running

Once everything has compile successfully, you can start an Erlang shell and
then bring the app up:

```bash
$ make shell
```

```erlang
application:load(clojurenode).
application:start(clojurenode).
```

Or, you can just use the ``dev`` make target:

```bash
$ make dev
```

Once the app has started, you will see output like the following (elided):

```
(erl_node@cahwsx01)1> src/clj_controller.erl:200:<0.44.0>:...
   starting clojure app with cmd "java -Dnode=\"clj_node@cahwsx01\" ...

src/clj_controller.erl:201:<0.44.0>: [{{2014,5,11},{22,37,1}}] ...

INFO: starting clojure app with cmd "java -Dnode=...
```

At this point, you are in the shell, and before too long you should also see
a log message display showing the successful start of the Clojure node:

```
src/clj_controller.erl:131:<0.44.0>: [{{2014,5,11},{22,37,6}}] ...
INFO: connection to java node established, pid <6709.1.0>
```

## Using

Now that you've got both ends of the connection up, you can take it for a
spin with a ping command:

```erlang
(erl_node@cahwsx01)1> erlang:send(
  {clj_mbox, clj_node@cahwsx01}, {ping, self()}).
{ping,<0.57.0>})
```

To see the response from the Clojure node, you'll need to flush the shell:

```erlang
(erl_node@cahwsx01)2> flush().
Shell got {pong,<6709.1.0>}
ok
```

Hurray! A successful response from the Clojure node!
