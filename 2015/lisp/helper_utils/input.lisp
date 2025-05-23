(in-package :helper-utils)

(defun read-input-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line collect line)
    )
  )

(defun string->chars (s)
   (coerce s 'list)
   )

(defun chars->string (char-list)
  (coerce char-list 'string)
  )

(defun split-string (s split-char)
  (let* ((chars (string->chars s))
         (result '())
         (current '())
         )
    (dolist (c chars)
      (if (char= c split-char)
          (progn
            (push (nreverse current) result)
            (setf current '())
            )
          (push c current)
          )
      )
    (push (nreverse current) result)
    (nreverse (mapcar #'chars->string result))
    )
  )
