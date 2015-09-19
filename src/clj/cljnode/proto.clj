(ns cljnode.proto
    #^{:authors ["Maxim Molchanov <elzor.job@gmail.com>",
                 "Duncan McGreggor <oubiwann@gmail.com>"],
       :doc "Protocol handler class"}
    (:require [clojure.tools.logging :as log])
    (:import [com.ericsson.otp.erlang
              OtpErlangAtom
              OtpErlangTuple
              OtpErlangObject])
    (:gen-class))

(defn handle-ping
    [msg mbox]
    (log/info (format "Handling %s ..." msg))
    (.send mbox (.elementAt ^OtpErlangTuple msg 1)
        (new OtpErlangTuple
            (into-array OtpErlangObject [(new OtpErlangAtom "pong") (.self mbox)]))))

(defn link-to-erl
    [dpid mbox]
    (.link mbox dpid)
    (log/info (format "Linked with Erlang %s" dpid)))

(defn check-erl-node
    [mbox timeout]
    (def msgObj (.receive mbox timeout))
    (def cmd (.elementAt msgObj 0))
    (def dpid (.elementAt msgObj 1))
    (if (= (.atomValue cmd) "ping")
        (link-to-erl dpid mbox)
        (new Exception "First message should be ping"))
    (log/info "erlang node checked"))
