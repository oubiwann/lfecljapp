(ns
    cljnode.core
    #^{:author "Maxim Molchanov <elzor.job@gmail.com>",
       :doc "Node run point"}
    (:require [clojure.tools.logging :as log]
              [cljnode.server :as srv])
    (:import [com.ericsson.otp.erlang
              OtpErlangAtom
              OtpErlangTuple
              OtpErlangObject
              OtpNode])
    (:gen-class))

(defn -main
    [& args]
    (log/info "start")
    (srv/server
        (System/getProperty "node")
        (System/getProperty "mbox")
        (System/getProperty "cookie")
        (read-string (System/getProperty "epmd_port")))
    (log/info "stop"))
