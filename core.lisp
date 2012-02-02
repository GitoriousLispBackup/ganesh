;; core functions for spaced repitition program

;; main storage for key-value pairs
(defparameter *key-value-hash* (make-hash-table))

;; add key-value pairs to *key-value-hash*
(defun enter-data ()
  (princ "Enter first value: ")
  (let ((k (read)))
    (princ "Enter second value: ")
    (let ((v (read)))
      (setf (gethash k *key-value-hash*) v))))

;; continue adding data to *key-value-hash*
(defun populate ()
  (loop initially
       (enter-data)
       while (y-or-n-p "Add another one?")
       do (enter-data)))

;; save data to external file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *key-value-hash* out))))

;; load data from external file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *key-value-hash* (read in)))))

;; show key-value pairs in main hash table
(defun show-hash ()
  (maphash (lambda (k v)
             (progn (format t "~a =? " k)
                    (read)
                    (format t "~a = ~a" k v)
                    (fresh-line)))
           *key-value-hash*))