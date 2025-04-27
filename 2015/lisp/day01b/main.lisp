(in-package :cl-user)

(defun map-parens (char)
  (cond ((char= char #\() 1)
        ((char= char #\)) -1)
        (t 0)))

(defun search-prefix (lst target n) 
  (if (> n (length lst))
      -1
      (let* ((prefix (subseq lst 0 n))
             (result (reduce #'+ prefix)))
        (if (equal result target)
            n
            (search-prefix lst target (1+ n))))))

(defun map-sum-parens (char-lines) 
  (reduce #'+ (mapcar #'map-parens (apply #'append char-lines)))
  )

(defun run (input-file)
  (let* ((input (helper-utils:read-input-lines input-file))
         (char-lines (mapcar (lambda (line) (coerce line 'list)) input)))
    (format t "Running with input from ~A~%Lines as chars: ~A~%" input-file (search-prefix (mapcar #'map-parens (apply #'append char-lines)) -1 1))
    char-lines)
  )  ; Return the list of lists of characters

