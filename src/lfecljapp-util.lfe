(defmodule lfecljapp-util
  (export (make-name 2)
          (ping 3)
          (start 0)))

(defun start ()
  (application:load 'lfecljapp)
  (application:start 'lfecljapp))

(defun ping (mbox recip sender)
  (erlang:send (tuple mbox recip)
               (tuple 'ping sender)))

(defun make-name (node host)
  (++ node "@" host))
