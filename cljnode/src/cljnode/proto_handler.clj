(ns
    cljnode.proto_handler
    #^{:author "Maxim Molchanov <elzor.job@gmail.com>",
       :doc "Protocol handler class"}
    (:require [clojure.tools.logging :as log])
    (:import [com.ericsson.otp.erlang
              OtpErlangAtom
              OtpErlangTuple
              OtpErlangObject
              OtpNode])
    (:gen-class))

(defn handle_ping
    [msg mbox]
    (log/info (format "handle %s" msg))
    (.send mbox (.elementAt ^OtpErlangTuple msg 1)
        (new OtpErlangTuple
            (into-array OtpErlangObject [(new OtpErlangAtom "pong") (.self mbox)]))))

(defn linkToErl
    [dpid mbox]
    (.link mbox dpid)
    (log/info (format "linked with erlang %s" dpid)))

(defn checkErlNode
    [mbox timeout]
    (def msgObj (.receive mbox timeout))
    (def cmd (.elementAt msgObj 0))
    (def dpid (.elementAt msgObj 1))
    (if (= (.atomValue cmd) "ping")
        (linkToErl dpid mbox)
        (new Exception "First message should be ping"))
    (log/info "erlang node checked"))
