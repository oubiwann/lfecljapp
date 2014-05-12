(defmodule lfecljapp-util
  (export (start 0)
          (ping 3)))

(defun start ()
  (application:load 'lfecljapp)
  (application:start 'lfecljapp))

(defun ping (mbox recip sender)
  (erlang:send (tuple mbox recip)
               (tuple 'ping sender)))
