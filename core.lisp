;; core functions for spaced repitition program

;; main storage for key-value pairs
(defparameter *main-list* nil)

;; container for today's cards
(defparameter *today-list* nil)

;; add key-value pairs to *main-list*
(defun enter-data (k v interval day)
  (list :k k :v v :interval interval :day day))

(defun prompt-for-data ()
  (princ "Enter first value: ")
  (let ((k (read-line)))
    (princ "Enter second value: ")
    (let ((v (read-line)))
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

;; returns interval of card
(defun card-interval (card)
  (getf card :interval))

;; returns universal-time of card
(defun card-day (card)
  (getf card :day))

;; show key-value pairs in main list
(defun show-cards ()
  (mapc (lambda (c)
          (progn (format t "~a = ? " (getf c :k))
                 (read)
                 (format t "~a = ~a" (getf c :k) (getf c :v))
                 (fresh-line)
                 (princ "(p)ass or (f)ail?")
                 (case (read)
                   (p (setf (getf c :interval) (1+ (card-interval c))))
                   (f (setf (getf c :interval)
                            (if (<= (card-interval c) 0)
                                0
                                (1- (card-interval c))))))
                 (adjust-date c)
                 (fresh-line)))
        *today-list*))

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

;; main loop
(defun main-loop()
  (loop initially
       (progn
         (date-compare)
         (show-cards))
       while (y-or-n-p "Again?")
       do (progn
            (date-compare)
            (show-cards))))
       