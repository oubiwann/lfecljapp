(defmodule lfeclj-util
  (export (make-name 2)
          (ping 3)))

(defun ping (mbox recip sender)
  (erlang:send (tuple mbox recip)
               (tuple 'ping sender)))

(defun make-name (node host)
  (++ node "@" host))
