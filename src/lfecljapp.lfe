(defmodule lfecljapp
  (behaviour gen_server)
  ;; API
  (export (start_link 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3))
  ;; Only for tests
  (export (stop 1)))

(include-lib "include/log.hrl")
(include-lib "eunit/include/eunit.hrl")

(defrecord state
  remote-pid
  (waiters '())
  ext-port-ref)

(defun server-name () (MODULE))
(defun ping-interval () 1000)
(defun FILE () (++ (atom_to_list (MODULE)) ".lfe"))
;;;===================================================================
;;; API
;;;===================================================================

;;--------------------------------------------------------------------
;; @doc
;; Starts the server
;;
;; @spec start_link() -> {ok, Pid} | ignore | {error, Error}
;; @end
;;--------------------------------------------------------------------
(defun start_link ()
  (gen_server:start_link
     (tuple 'local (server-name)) (MODULE) '() '()))

;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Initializes the server
;;
;; @spec init(Args) -> {ok, State} |
;;                     {ok, State, Timeout} |
;;                     ignore |
;;                     {stop, Reason}
;; @end
;;--------------------------------------------------------------------
(defun init (args)
  (let ((port (start-app)))
    (gen_server:cast (self) 'ping)
    (tuple 'ok (make-state ext-port-ref port))))

;; only for testing
(defun stop (reason)
  (gen_server:cast (server-name) (tuple 'stop-test reason)))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling call messages
;;
;; @spec handle_call(Request, From, State) ->
;;                                   {reply, Reply, State} |
;;                                   {reply, Reply, State, Timeout} |
;;                                   {noreply, State} |
;;                                   {noreply, State, Timeout} |
;;                                   {stop, Reason, Reply, State} |
;;                                   {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_call (_ _ state)
  (tuple 'reply 'ok state))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling cast messages
;;
;; @spec handle_cast(Msg, State) -> {noreply, State} |
;;                                  {noreply, State, Timeout} |
;;                                  {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_cast
  (((tuple 'stop-test reason) state)
   (tuple 'stop reason state))
  (('ping state)
   (let (((tuple 'ok node) (application:get_env 'lfecljapp 'node))
         ((tuple 'ok mbox) (application:get_env 'lfecljapp 'mbox))
         ((tuple 'ok host) (case (application:get_env 'lfecljapp 'host)
                             ('undefined (inet:gethostname))
                             (other other))))
     (ping host node mbox)
     (erlang:send_after (ping-interval) (self) 'ping)
     (tuple 'noreply state)))
  ((message state)
   (ERROR "unhandled case, ~p" (list message))
   (tuple 'noreply state)))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling all non call/cast messages
;;
;; @spec handle_info(Info, State) -> {noreply, State} |
;;                                   {noreply, State, Timeout} |
;;                                   {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_info

;; Erlang test for first
;; lfecljapp:handle_info(ping, {state,undefined,[],233}).
;; {noreply,{state,undefined,[],undefined}}
;; flush().
;; Shell got {'$gen_cast',ping}
;;
;; LFE test for first
;; (handle_info 'ping (make-state waiters (1 2 3) ext-port-ref 456))
  (('ping (= (match-state remote-pid 'undefined) state))
    `#(noreply ,state))
;; Erlang test for second
;; lfecljapp:handle_info(ping, {state,"apples",[],123}).
;; {noreply,{state,"apples",[],123}}
;;
;; LFE test for second
;; (handle_info 'ping (make-state remote-pid 123))
  (('ping state)
   `#(noreply ,state))
;; LFE test for third
;; (handle_info #(pong 798) (make-state waiters (1 2 3) ext-port-ref 456))
  (((tuple 'pong pid) (= (match-state remote-pid 'undefined
                                      waiters waiters)
                         state))
   (INFO "Connection to java node established, pid: '~p'" (list pid))
   (link pid)
   (lists:foreach
     (lambda (x)
       (gen_server:cast (self) `#(wait-for-login x)))
     waiters)
   `#(noreply ,(make-state remote-pid pid
                           waiters waiters
                           ext-port-ref (state-ext-port-ref))))
;; Erlang test for fourth
;; lfecljapp:handle_info({pong, "oranges"}, {state,"apples",[],undefined}).
;; {noreply,{state,"apples",[],undefined}}
;;
;; LFE test for fourth
;; (handle_info #(pong 798) (make-state remote-pid 123))
  (((tuple 'pong _) state)
   `#(noreply ,state))
;; Erlang test for fifth
;; lfecljapp:handle_info({234,{exit-status,"success"}}, {state,123,[],234}).
;; ERROR: external java app exited with status "success"
;; {stop,{error,{java_app_exit,"success"}},{state,123,[],234}}
;;
;; LFE tests for fifth
;; (handle_info #(567 #(exit-status "bad calc")) (make-state ext-port-ref 567))
  (((tuple port (tuple 'exit-status status)) (= (match-state ext-port-ref ext-port)
                                                state)) (when (== port ext-port))
   (ERROR "External java app exited with status: '~p'" (list status))
   `#(stop #(error #(java-app-exit ,status)) ,state))
;; Erlang test for sixth
;; lfecljapp:handle_info({'EXIT',123,"run error"}, {state,123,[],undefined}).
;; ERROR: external java mbox exited with reason "run error"
;; {stop,{error,{java_mbox_exit,"run error"}},
;;       {state,123,[],undefined}}
;;
;; LFE tests for sixth
;; (handle_info #(EXIT 567 "bad calc") (make-state remote-pid 567))
  (((tuple 'EXIT pid reason) (= (match-state remote-pid remote-pid)
                                state)) (when (== pid remote-pid))
   (ERROR "External java mbox exited with reason: '~p'" (list reason))
   `#(stop #(error #(java-mbox-exit ,reason)) ,state))
;; Erlang test for seventh
;; lfecljapp:handle_info(weee, {state,"nothing here",[],undefined}).
;; ERROR: unhandled info, weee
;; {noreply,{state,"nothing here",[],undefined}}
;;
  ((info state)
   (ERROR "Unhandled info: '~p'" (list info))
   `#(noreply ,state)))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; This function is called by a gen_server when it is about to
;; terminate. It should be the opposite of Module:init/1 and do any
;; necessary cleaning up. When it returns, the gen_server terminates
;; with Reason. The return value is ignored.
;;
;; @spec terminate(Reason, State) -> void()
;; @end
;;--------------------------------------------------------------------
(defun terminate (reason state)
  'ok)

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Convert process state when code is changed
;;
;; @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
;; @end
;;--------------------------------------------------------------------
(defun code_change (old-version state extra)
  (tuple 'ok state))

;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun start-app ()
  (let* (((tuple 'ok node-cfg) (application:get_env 'lfecljapp 'node))
         ((tuple 'ok host-name) (inet:gethostname))
         (node (full-node-name host-name node-cfg))
         ((tuple 'ok mbox) (application:get_env 'lfecljapp 'mbox))
         ((tuple 'ok cmd) (application:get_env 'lfecljapp 'cmd))
         ((tuple 'ok port) (application:get_env 'lfecljapp 'epmd_port))
         (priv-dir (code:priv_dir 'lfecljapp))
         (log-file-name (++ (atom_to_list node) "_clj.log"))
         (full-cmd (++ "java -Dnode=\"" (atom_to_list node) "\" -Dmbox=\""
                       (atom_to_list mbox) "\" -Dcookie=\""
                       (atom_to_list (erlang:get_cookie)) "\" -Dempd_port="
                       (lists:flatten (io_lib:format "~p" (list port)))
                       " -Dlogfile=\"" priv-dir "/" log-file-name
                       "\" -classpath " priv-dir "/" cmd " ")))
    (INFO "starting clojure app with cmd ~p" (list full-cmd))
    (open_port `#(spawn ,full-cmd) 'exit-status)))

(defun ping (host node mbox)
  (lfecljapp-util:ping mbox (full-node-name host node) (self)))

(defun full-node-name (host node)
  (list_to_atom (++ (atom_to_list node) "@" host)))
