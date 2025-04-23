(require :asdf)

(pushnew (truename ".") asdf:*central-registry*)
(load (merge-pathnames "../helper_utils/helper-utils.asd" (truename ".")))

(declaim (notinline run)) ; <-- suppress compile-time warnings

(defun get-args ()
  (remove "--" (uiop:command-line-arguments) :test #'string=))

(defun launch-day ()
  (let* ((args (get-args))
         (input-file (if args (first args) "input.txt")))
    (asdf:load-system :aoc-day)
    (run input-file)))

(launch-day)

