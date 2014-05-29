(defmodule lfeclj-sup
  (behaviour supervisor)
  ;; API
  (export (start_link 0))
  ;; Supervisor callbacks
  (export (init 1)))

(defun server-name ()
  'lfeclj-sup)

(defun start_link ()
  (supervisor:start_link
    (tuple 'local (server-name)) (MODULE) '()))

(defun init (args)
  (let* ((server (tuple
                   'lfecljapp
                   (tuple 'lfecljapp 'start_link '())
                   'permanent
                   5000
                   'worker
                   (list 'lfecljapp)
                   ))
         (children (list server))
         (restart-strategy (tuple 'one_for_one 5 10)))
    (tuple 'ok (tuple restart-strategy children))))
