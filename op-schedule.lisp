(defclass day ()
	   ((date :accessor date
		  :initform nil
		  :initarg :date)))

(defclass op-day (day)
	   ((slot-1 :accessor slot-1
		    :initform nil
		    :initarg :slot-1)
	    (slot-2 :accessor slot-2
		    :initform nil
		    :initarg :slot-2)))

(defclass great-op-day (op-day)
	   ((slot-3 :accessor slot-3
		    :initform nil
		    :initarg :slot-3)))

(defclass month ()
	   ((day-length :accessor day-length
		    :initform 30
		    :initarg :day-length)
            (year :accessor year
                  :initform 1900
                  :initarg :year)
	    (month-number :accessor month-number
			  :initform 1
			  :initarg :month-number)
	    (initial-weekday :accessor initial-weekday
			     :initform 0
			     :initarg :initial-weekday)
	    (days :accessor days
                  :initform nil)))


;;Initializer of an instance of month
;;It sets up the list of days according to the length of the month with appropriate
;;days, op-days, and great-op-days

(defmethod initialize-instance :after ((m month) &key)
  (if (leap-year-p (year m))
      (setf day-length (aref *day-length-of-month-leap* (- (month-number m) 1)))
      (setf day-length (aref *day-length-of-month-non-leap* (- (month-number m) 1))))
  (dotimes (n day-length)
    (let ((current-date (simple-date:encode-date
                         (year m) (month-number m) (+ n 1))))
      (cond
        ((not (op-day-p current-date))
         (cons (make-instance 'day :date current-date) (days m)))
        ((not (great-op-day-p current-date))
         (cons (make-instance 'op-day :date current-date) (days m)))
        (t
         (cons (make-instance 'great-op-day :date current-date))))
      (setf (days m) (reverse (days m))))))


(defun op-day-p (date)
  (case (simple-date:day-of-week date)
    ((1 2 3) t)
    (t nil)))


;;Great-op-day is a predicate function to tell whether the given date is a
;;great-op-day or not
;;The first and the third Wednesday are the great-op-days
;;This function depends on the *first-wednesday-conversion-array* for calculation

(defun great-op-day-p (date)
  (multiple-value-bind (year month day)
      (simple-date:decode-date date)
    (let ((weekday-of-day1 (day-of-week 1 month year)))
      (if (member (- day 1) (aref *first-wednesday-conversion-array* weekday-of-day1)
                  :test #'=)
          t
          nil))))

;;Array of the big-op-day according to the week-day of the first day of the month
;;The returned day is zero-started mode

(setf *first-wednesday-conversion-array*
      #((2 16) (1 15) (0 14) (6 20) (5 19) (4 20) (3 17)))

;;Array of the length of each month of a leap year

(setf *day-length-of-month-leap* 
      #(31 20 31 30 31 30 31 31 30 31 30 31))

;;Array of the length of each month of a non-leap year

(setf *day-length-of-month-non-leap* 
      #(31 28 31 30 31 30 31 31 30 31 30 31))


;;Leap-year-p is a predicate function to tell whether a given year
;;is a leap year or not

(defun leap-year-p (year)
  (if (= (mod year 4) 0) ;if divisible with 4, leap year
      (if (= (mod year 100) 0)
          (if (= (mod year 400) 0)
              t
              nil)
          nil) 
      nil))  ;not a leap year if not divisible with 4


(defclass op ()
	   ((patient-id :accessor patient-id
			:initform nil
			:initarg :patient-id)
	    (diagnosis :accessor diagnosis
		       :initform nil
		       :initarg :diagnosis)
            (procedure :accessor procedure
                       :initform nil
                       :initarg :procedure)
	    (surgeon :accessor surgeon
		     :initform nil
		     :initarg :surgeon)
	    (op-time :accessor op-time
		     :initform nil
		     :initarg :op-time)))

(defmethod set-slot-1 ((od op-day) (o op))
    (setf (slot-1 od) o))

(defmethod set-slot-2 ((od op-day) (o op))
  (setf (slot-2 od) o))

(defmethod delete-slot-1 ((od op-day))
  (setf (slot-1 od) nil))

(defmethod delete-slot-2 ((od op-day))
  (setf (slot-2 od) nil))

