(ns cljnode.core
    #^{:authors ["Maxim Molchanov <elzor.job@gmail.com>",
                 "Duncan McGreggor <oubiwann@gmail.com>"],
       :doc "Node run point"}
    (:require [clojure.tools.logging :as log]
              [cljnode.server :as cljnode])
    (:gen-class))

(defn -main
    [& args]
    (log/info "Starting Clojure node ...")
    (cljnode/server
        (System/getProperty "node")
        (System/getProperty "mbox")
        (System/getProperty "cookie")
        (read-string (System/getProperty "epmd_port")))
    (log/info "Stopping Clojure node ..."))
