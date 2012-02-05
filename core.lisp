;; core functions for spaced repitition program

;; main storage for key-value pairs
(defparameter *key-value-hash* (make-hash-table))

;; add key-value pairs to *key-value-hash*
(defun enter-data ()
  (princ "Enter first value: ")
  (let ((k (read-line)))
    (princ "Enter second value: ")
    (let ((v (read-line)))
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

;; calculate interval, based on supermemory-0 algorithm (http://www.supermemo.com/english/ol/beginning.htm#Algorithm)
(defun interval (val)
  (cond ((eq val 1)
         1)
        ((eq val 2)
         7)
        ((eq val 3)
         16)
        ((eq val 4)
         35)
        (t (* (interval (1- val)) 2))))