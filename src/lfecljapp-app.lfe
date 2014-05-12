(defmodule lfecljapp-app
  (behaviour application)
  (export (start 2)
          (stop 1)))

(defun start (type args)
  (let ((result (lfecljapp-sup:start_link)))
    (case result
      ((tuple 'ok pid)
        result)
      (_
        (tuple 'error result)))))

(defun stop (state)
  'ok)
