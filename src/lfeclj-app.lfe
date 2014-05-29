(defmodule lfeclj-app
  (behaviour application)
  (export (start 0)
          (start 2)
          (stop 1)))

(defun start ()
  (application:load 'lfecljapp)
  (application:start 'lfecljapp))

(defun start (type args)
  (let ((result (lfeclj-sup:start_link)))
    (case result
      ((tuple 'ok pid)
        result)
      (_
        (tuple 'error result)))))

(defun stop (state)
  'ok)
