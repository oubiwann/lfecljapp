(ns
    cljnode.server
    #^{:author "Maxim Molchanov <elzor.job@gmail.com>",
       :doc "Main server class"}
    (:require [clojure.tools.logging :as log]
              [cljnode.proto_handler :as proto])
    (:import [com.ericsson.otp.erlang
              OtpErlangAtom
              OtpErlangTuple
              OtpErlangObject
              OtpNode])
    (:gen-class))

(defn process
    [msg mbox]
    (def cmd (.elementAt ^OtpErlangTuple msg 0))
    (cond
        (= (.atomValue cmd) "ping") (proto/handle_ping msg mbox)
        :else (log/error (format "undefined msg: %s" (str msg)))))

(defn handleErlMessages
    [mbox]
    (try (def msg (.receive mbox 50))
         (if (instance? OtpErlangTuple msg) (process msg mbox) ())
         (handleErlMessages mbox)
    (catch Exception e (log/error (format (str e))))))

(defn server
    [NodeName Mbox Cookie Port]
    (log/info (format "started with params:\n\tnodename: %s\n\tmbox: %s\n\tcookie: %s\n\tepmd_port: %s"
                NodeName Mbox Cookie Port))
    (def mbox (.createMbox (new OtpNode NodeName Cookie Port) Mbox))
    (proto/checkErlNode mbox 10000)
    (trampoline handleErlMessages mbox)
    (log/info "destroy server"))
