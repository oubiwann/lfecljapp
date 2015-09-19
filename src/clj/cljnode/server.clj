(ns cljnode.server
    #^{:authors ["Maxim Molchanov <elzor.job@gmail.com>",
                 "Duncan McGreggor <oubiwann@gmail.com>"],
       :doc "Main server class"}
    (:require [clojure.tools.logging :as log]
              [cljnode.proto :as proto])
    (:import [com.ericsson.otp.erlang
              OtpErlangTuple
              OtpNode])
    (:gen-class))

(defn process
    [msg mbox]
    (def cmd (.elementAt ^OtpErlangTuple msg 0))
    (cond
        (= (.atomValue cmd) "ping") (proto/handle-ping msg mbox)
        :else (log/error (format "Undefined msg: %s" (str msg)))))

(defn handle-erl-messages
    [mbox]
    (try (def msg (.receive mbox 50))
         (if (instance? OtpErlangTuple msg) (process msg mbox) ())
         #(handle-erl-messages mbox)
    (catch Exception e (log/error (format (str e))))))

(defn server
    [NodeName Mbox Cookie Port]
    (log/info (format "Started with params:\n\tnodename: %s\n\tmbox: %s\n\tcookie: %s\n\tepmd_port: %s"
                NodeName Mbox Cookie Port))
    (def mbox (.createMbox (new OtpNode NodeName Cookie Port) Mbox))
    (proto/check-erl-node mbox 10000)
    (trampoline handle-erl-messages mbox)
    (log/info "Terminating Clojure node server ..."))
