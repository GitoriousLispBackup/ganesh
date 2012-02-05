;; test module

(load "core.lisp")

;; database test
(defun test-db ()
  (progn
    (setf (gethash 'test *key-value-hash*) 'good)
    (save-db "test.lisp")
    (setf *key-value-hash* nil)
    (load-db "test.lisp")
    (equal (gethash 'test *key-value-hash*) 'good)))

(defun test-interval ()
  (eq (interval 5) 70)
  (eq (interval 6) 140))
  