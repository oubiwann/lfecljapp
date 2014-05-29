(defmodule lfecljapp-app
  (behaviour application)
  (export (start 2)
          (stop 1)))

(defun start (type args)
  (let ((result (lfecljapp-sup:start_link)))
    (case result
      ((tuple 'ok pid)
        result)
      (_
        (tuple 'error result)))))

(defun stop (state)
  'ok)

; "java -Dnode=\"clj_node@cahwsx01\"
;       -Dmbox=\"clj_mbox\"
;       -Dcookie=\"RYIGBNOJUYOKPMJFTFHY\"
;       -Depmd_port=15000
;       -Dlogfile=\"/Users/oubiwann/lab/erlang/lfecljapp/priv/erl_node@cahwsx01_clj.log\"
;       -classpath /Users/oubiwann/lab/erlang/lfecljapp/priv/cljnode-0.1.1-standalone.jar
;       cljnode.core"

; "java -Dnode=\"clj_node@cahwsx01\"
;       -Dmbox=\"clj_mbox\"
;       -Dcookie=\"RYIGBNOJUYOKPMJFTFHY\"
;       -Dempd_port=15000
;       -Dlogfile=\"/Users/oubiwann/lab/erlang/lfecljapp/priv/clj_node@cahwsx01_clj.log\"
;       -classpath /Users/oubiwann/lab/erlang/lfecljapp/priv/cljnode-0.1.1-standalone.jar
;       cljnode.core "
