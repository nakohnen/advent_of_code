(in-package :helper-utils)

(defun read-input-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line collect line)))

