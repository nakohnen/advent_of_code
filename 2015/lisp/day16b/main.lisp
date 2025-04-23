(in-package :cl-user)

(defun run (input-file)
  (let ((input (helper-utils:read-input-lines input-file)))
    (format t "Running with input from ~A~%First line: ~A~%" input-file (first input))))

