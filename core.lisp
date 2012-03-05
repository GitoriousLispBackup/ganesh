;; core functions for spaced repitition program

;; finish-output and read wrapper function
(defun user-input ()
  (finish-output nil)
  (read))

;;;; input and storage
;; main storage for key-value pairs
(defparameter *main-list* nil)

;; container for today's cards
(defparameter *today-list* nil)

;; loaded database file
(defparameter *db* nil)

;; add key-value pairs to *main-list*
(defun enter-data (k v interval day)
  (list :k k :v v :interval interval :day day))

(defun prompt-for-data ()
  (princ "Enter first value: ")
  (let ((k (user-input)))
    (princ "Enter second value: ")
    (let ((v (user-input)))
      (push (enter-data k v 0 (get-universal-time)) *main-list*))))

;; continue adding data to *main-list*
(defun populate ()
  (loop initially
       (prompt-for-data)
     while (y-or-n-p "Add another one?")
     do (prompt-for-data)))

;; save data to external file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *main-list* out))))

;; load data from external file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *main-list* (read in)))))

;; wrapper function for load-db
(defun which-db ()
  (let ((num 0)
        (files (loop for f in (directory (pathname "db/*.db"))
                    collect f)))
    (loop for f in files
       do (format t "~d. ~a ~%" num (pathname-name f))
         (incf num))
    (princ "Open file: ")
    (let ((x (user-input)))
      (setf *db* (nth x files))
      (load-db (nth x files)))))

;;;; data access
;; returns interval of card
(defun card-interval (card)
  (getf card :interval))

;; returns universal-time of card
(defun card-day (card)
  (getf card :day))

;; shuffle cards
(defun shuffle-cards (cards)
  (let ((new-lst nil))
    (labels ((shuffle (lst)
               (when lst
                 (push (nth (random (length lst))
                            lst)
                       new-lst)
                 (shuffle (concatenate 'list (subseq lst 0 (position (car new-lst) lst))
                                       (subseq lst (1+ (position (car new-lst) lst))))))))
      (shuffle cards))
    new-lst))
  
;; show key-value pairs in main list
(defun show-cards ()
  (if *today-list*
      (mapc (lambda (c)
              (progn (format t "~a " (getf c :k))
                     (user-input)
                     (format t "~a " (getf c :v))
                     (fresh-line)
                     (princ "(p)ass or (f)ail?")
                     (let ((x (user-input)))
                       (adjust-interval x c))
                     (adjust-date c)
                     (fresh-line)))
            (shuffle-cards *today-list*))
      (princ "No due cards.")))

;; seconds in a day
(defun secs->day ()
  (* (* 60 60) 24))

;; calculates new interval in seconds of a day
(defun time-calc (c)
  (* (secs->day)
     (interval (card-interval c))))

;; adjust card due dates
(defun adjust-date (c)
  (setf (getf c :day) (+ (card-day c)
                         (time-calc c))))

;; populate *today-list* with due cards
(defun date-compare ()
  (setf *today-list* (remove-if-not (lambda (c)
                                      (let ((date (card-day c)))
                                        (>= (get-universal-time)
                                            date)))
                                    *main-list*)))

;; invert the cards' keys and values
(defun card-invert ()
  (mapc (lambda (c)
          (psetf (getf c :k) (getf c :v)
                 (getf c :v) (getf c :k)))
        *main-list*))

;; edits selected card, selection done either through search or selected from a list of all cards
(defun edit-card (c k v)
  (setf (getf c :k) k)
  (setf (getf c :v) v))

;;;; algorithm
;; calculate interval, based on supermemory-0 algorithm (http://www.supermemo.com/english/ol/beginning.htm#Algorithm)
(defun interval (val)
  (cond ((eq val 0)
         0)
        ((eq val 1)
         1)
        ((eq val 2)
         7)
        ((eq val 3)
         16)
        ((eq val 4)
         35)
        (t (* (interval (1- val)) 2))))

;; adjusts individual card interval based on the pass or fail answer
(defun adjust-interval (mark card)
  (case mark
    (p (setf (getf card :interval) (1+ (card-interval card))))
    (f (setf (getf card :interval)
             (if (<= (card-interval card) 0)
                 0
                 (1- (card-interval card)))))
    (otherwise (princ "Please enter 'p' or 'f': ")
               (adjust-interval (read) card))))

;;;; custom repl
(defparameter *allowed-commands* '(new-db study add edit quit))

(defun custom-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      (format t "Unknown command ~%")))

(defun custom-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    cmd))

(defun custom-loop ()
  (format t "The commands are: ~{ ~<~a~> ~} ~%" *allowed-commands*)
  (let ((cmd (custom-read)))
    (unless (eq (car cmd) 'quit)
      (custom-eval cmd)
      (custom-loop))))

;; wrapper function
(defun new-db ()
  (setf *main-list* nil)
  (populate)
  (princ "Enter filename:")
  (save-db (pathname (concatenate 'string "db/" (user-input) ".db"))))
   
;; wrapper function
(defun study ()
  (main-loop)
  (save-db *db*))

;; wrapper function
(defun add ()
  (which-db)
  (populate)
  (save-db *db*))

(defun edit ()
  (let ((i 0))
    (loop for c in *main-list*
         do (format t "~d. ~a = ~a ~%" i (getf c :k) (getf c :v))
         (incf i)))
  (princ "Choose a card:")
  (let ((i (user-input)))
    (princ "Enter first value:")
    (let ((k (user-input)))
      (princ "Enter second value:")
      (let ((v (user-input)))
        (edit-card (nth i *main-list*)
                   k v)))))


;; main loop
(defun main-loop()
  (which-db)
  (format t "Invert the cards?")
  (finish-output nil)
  (if (y-or-n-p)
      (card-invert))
  (loop initially
       (progn
         (date-compare)
         (show-cards))
       while (y-or-n-p "Again?")
       do (progn
            (setf *today-list* nil)
            (date-compare)
            (show-cards))))
       