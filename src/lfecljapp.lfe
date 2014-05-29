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
  (export (stop 1)
          (ping 2)
          (ping 3)))

(include-lib "include/log.hrl")
(include-lib "eunit/include/eunit.hrl")

(defrecord state
  remote-pid
  (waiters '())
  ext-port-ref)

(defun server-name () (MODULE))
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
  (let ((port (start-clojure)))
    (gen_server:cast (self) 'ping)
    (tuple 'ok (make-state ext-port-ref port))))

;; only for testing
(defun stop (reason)
  (gen_server:cast (server-name) (tuple 'stop-test reason)))

(defun ping (node mbox)
  (lfeclj-util:ping
    (list_to_atom mbox)
    (list_to_atom node)
    (self)))

(defun ping (host node mbox)
  (ping (lfeclj-util:make-name node host) mbox))

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
   (let ((node (lfeclj-cfg:get 'node))
         (mbox (lfeclj-cfg:get 'mbox))
         (host (case (lfeclj-cfg:get 'host)
                 ('undefined (element 2 (inet:gethostname)))
                 (other other))))
     (ping host node mbox)
     (erlang:send_after (lfeclj-cfg:get 'ping-interval)
                        (self)
                        'ping)
     (tuple 'noreply state)))
  ((message state)
   (ERROR "Unhandled case: '~p'" (list message))
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
  (('ping (= (match-state remote-pid 'undefined) state))
    (gen_server:cast (self) 'ping)
    `#(noreply ,state))
  (('ping state)
   `#(noreply ,state))
  (((tuple 'pong pid) (= (match-state remote-pid 'undefined
                                      waiters waiters)
                         state))
   (INFO "Connection to java node established, pid: ~p" (list pid))
   (link pid)
   (lists:foreach
     (lambda (x)
       (gen_server:cast (self) `#(wait-for-login x)))
     waiters)
   `#(noreply ,(make-state remote-pid pid
                           waiters waiters
                           ext-port-ref (state-ext-port-ref))))
  (((tuple 'pong _) state)
   `#(noreply ,state))
  (((tuple port (tuple 'exit_status status)) (= (match-state ext-port-ref ext-port)
                                                state)) (when (== port ext-port))
   (ERROR "External java app exited with status: '~p'" (list status))
   `#(stop #(error #(java-app-exit ,status)) ,state))
  (((tuple 'EXIT pid reason) (= (match-state remote-pid remote-pid)
                                state)) (when (== pid remote-pid))
   (ERROR "External java mbox exited with reason: '~p'" (list reason))
   `#(stop #(error #(java-mbox-exit ,reason)) ,state))
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

(defun start-clojure ()
  (let* ((node (lfeclj-cfg:get 'node))
         ((tuple 'ok host) (inet:gethostname))
         (node-name (lfeclj-util:make-name node host))
         (cmd (lfeclj-cfg:get 'cmd))
         (priv-dir (code:priv_dir 'lfecljapp))
         (log-file-name (++ (atom_to_list (node)) "_clj.log"))
         (full-cmd (++ "java -Dnode=\"" node-name
                       "\" -Dmbox=\"" (lfeclj-cfg:get 'mbox)
                       "\" -Dcookie=\"" (atom_to_list (erlang:get_cookie))
                       "\" -Depmd_port=" (lfeclj-cfg:get 'epmd-port)
                       " -Dlogfile=\"" priv-dir "/" log-file-name
                       "\" -classpath " priv-dir "/" cmd " ")))
    (INFO "Starting clojure app with cmd: ~p" (list full-cmd))
    (open_port `#(spawn ,full-cmd) (list 'exit_status))))

