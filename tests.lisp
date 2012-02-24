;; test module

(load "core.lisp")

;; test template
;; function expected-value -> pass or fail
(defun run-test (fn val)
  (let ((func-val (funcall fn)))
  (if (equal func-val val)
      (format t "~a test passed. ~%" fn)
      (format t "~a test failed, value is ~a ~%" fn func-val))))

;; enter-data function test
(defun test-input ()
  (push (enter-data 'doko 'where 0 (get-universal-time)) *main-list*)
  (getf (car *main-list*) :k))

(run-test #'test-input 'doko)
                        
;; save-db and load-db function test
(defun test-db ()
  (progn
    (push (enter-data 'doko 'where 0 (get-universal-time)) *main-list*)
    (save-db "test.lisp")
    (setf *main-list* nil)
    (load-db "test.lisp")
    (getf (car *main-list*) :v)))

(run-test #'test-db 'where)

;; card-interval function test
(run-test #'(lambda ()
              (card-interval (enter-data 'doko 'where 1 (get-universal-time))))
          1)

;; adjust-interval function test
(run-test #'(lambda ()
              (progn
                (let ((*main-list* (enter-data 'doko 'where 0 0))) 
                  (adjust-interval 'p (car *main-list*))
                  (getf (car *main-list*) :interval))))
          1)

;; secs->day function test
(run-test #'secs->day 86400)

;; interval function test
(run-test #'(lambda ()
              (interval 5))
          70)

;; card-invert function test
(run-test #'(lambda ()
              (progn (card-invert)
                     (getf (car *main-list*) :k)))
          'where)

(run-test #'(lambda ()
              (progn
                (edit-card (car *main-list*) 'doko 'where)
                (getf (car *main-list*) :k)))
          'doko)