;;;;This file contains the functions to be used for the renovation of the
;;;;patient database.

(require 'postmodern)
(require 'cl-ppcre)


(defun patient-with-multiple-diag ()
  "This function returns the list of patient_id who have multiple diagnoses"
  (mapcar 'car
          (postmodern:query (:select 'patient_id :from 'pt_diag
                                                 :group-by 'patient_id
                                                 :having (:> (:raw "count(*)") 1)))))

(defun patient-with-multiple-op ()
  "List the patient_id who have larger thatn one operation"
	   (remove-if-not #'(lambda (x) (> (get-number-op x) 1))
			  (mapcar #'car (postmodern:query
					 (:select 'patient_id
					  :from 'patient)))))

(defun patient-with-diag-num (n)
  "This function returns the list of patient_id who have n number of diagnoses"
  (mapcar 'car
          (postmodern:query (:select 'patient_id :from 'pt_diag
                                                 :group-by 'patient_id
                                                 :having (:= (:raw "count(*)") n)))))

(defun patient-with-op-num (n)
  "List the patient_id who have n number of operations"
	   (remove-if-not #'(lambda (x) (= n (get-number-op x)))
			  (mapcar #'car (postmodern:query
					 (:select 'patient_id
					  :from 'patient)))))


(defun get-number-op (patient_id)
  "This function returns the number of operation that the patient_id had in the database"
  (let ((tempnum))
    (if (setf tempnum
              (caar (postmodern:query (:select (:raw "count(o.op_id)")
                                       :from (:as 'op 'o)
                                       :left-join (:as 'patient 'p)
                                       :on (:= 'p.patient_id 'o.patient_id)
                                       :where (:= 'p.patient_id patient_id)
                                       :group-by 'p.patient_id))))
        tempnum
        0)))

(defun search-op-with-key (&key proc diag indic opnote year)
  "search the op table for keywords in procedure and/or preop diagnosis"
	   (mapcar #'(lambda (x)
                       (list (first x) (second x) (third x)))
                                        ;show only the first two items
                   (remove-if-not
                    #'(lambda (x)
                        (let ((b (second x))
                              (c (third x))
                              (d (fourth x))
                              (e (fifth x))
                              (f (sixth x)))
                          (and
                           (if proc
                               (scanstring-ex-nul proc b)
                               t)
                           (if diag
                               (scanstring-ex-nul diag c)
                               t)
                           (if indic
                               (scanstring-ex-nul indic d)
                               t)
                           (if opnote
                               (scanstring-ex-nul opnote e)
                               t)
                           (if year
                               (match-year year f)
                               t))))
                    (postmodern:query
                     (:select 'op_id 'procedure 'preop_dx 'indication
                              'op_note 'op_date
                              :from 'op)))))

(defun scanstring-ex-nul (key string)
  (if (not (equal string :null))
      (cl-ppcre:scan (cl-ppcre:create-scanner key :CASE-INSENSITIVE-MODE t) string)
      nil))

(defun match-year (year postgre-date)
  "Return t if the year matches the date-type output of the postgres query"
  (if (not (equal postgre-date :null))
      (= year (date-to-year postgre-date))
      nil))

(defun list-keys (op-id)
  (let ((lst))
    (mapcar #'(lambda (x) (setf lst (adjoin x lst)))
            (mapcar 'car (postmodern:query (:select 'key :from 'op_key
                                                 :where (:= 'op_id op-id))))) lst))


(defun insert-op-disease-id (op-id disease-id)
  "Insert into op_diag table the data: op-id and disease-id"
  (unless (member disease-id (op.disease-id-lst op-id))
    (postmodern:execute (:insert-into 'op_diag :set 'op_id op-id 'disease_id disease-id))))

(defun insert-key (op-id key)
  "insert into the op_key table the pair of op-id and keyword"
  (unless (member key (list-keys op-id) :test #'string= )
      (postmodern:execute (:insert-into 'op_key :set 'op_id op-id 'key key))))

(defun insert-key-to-lst (key lst)
  "insert the keyword to the table op_key for the list of the op_id"
	   (mapcar #'(lambda (x) (insert-key x key)) lst))

(defun keys-of-op (op-id)
  "return the list of keywords associated with the op-id"
  (let ((lst))
    (mapcar #'(lambda (x) (setf lst (adjoin x lst :test #'string= )))
            (mapcar 'car (postmodern:query (:select 'key :from 'op_key
                                                 :where (:= 'op_id op-id)))))
    lst))

(defmacro opdesc-for-key (&rest keys)
  "Obtain op_id, procedure, preop_dx for the set of search keys"
	   `(apply #'search-op-with-key (quote ,keys)))

(defmacro op-id-for-key (&rest keys)
  "In contrast to opdesc-for-key, this function returns
the list of op_id matching the given key combination"
  `(mapcar #'car (apply #'search-op-with-key (quote ,keys))))

(defun search-op-keyword (keyword)
  "Returns the list of op_id which has the exact keywored given in the parameter"
  (mapcar #'car (postmodern:query (:select 'op_id :from 'op_key
                                           :where (:= 'key keyword)))))

(defun majordiv-of-pt (patient-id)
  "Returns the list of major div of the diagnoses associated with the patient_id"
	   (mapcar #'(lambda (x)
		       (strip-white-space (car x)))
		   (postmodern:query
		    (:select 'md.major_div
		     :from (:as 'majordiv 'md)
		     :left-join (:as 'diagnosis 'd)
		     :on (:= 'd.major_div_id 'md.major_div_id)
		     :left-join (:as 'pt_diag 'pd)
		     :on (:= 'pd.disease_id 'd.disease_id)
		     :left-join (:as 'patient 'p)
		     :on (:= 'pd.patient_id 'p.patient_id)
		     :where (:= 'p.patient_id patient-id)))))


(defun list-diagnum-opnum (dnum opnum)
  (mapcar #'(lambda (x)
              (list (car (op-list-of-pt x)) (car (majordiv-of-pt x))))
          (intersection (patient-with-diag-num dnum) (patient-with-op-num opnum)
		       :test 'string=)))

(defun op-diag-present-p (op-id)
  "True if the op-id has an entry in the op_diag table"
	   (car (postmodern:query (:select 'op_id
					   :from 'op_diag
					   :where (:= 'op_id op-id)))))

(defun all-op ()
  "Return all the op_id"
	   (postmodern:query (:select 'op_id
					   :from 'op)))

;;Functions for patient or op classes

(defun op.get-preopdx (op-id)
	   (caar (postmodern:query (:select 'preop_dx
				      :from 'op
				      :where (:= 'op_id op-id)))))

(defun op.disease-id-lst (op-id)
  "Return the list of disease-id related to the given op-id"
  (let ((lst))
    (mapcar #'(lambda (x) (setf lst (adjoin x lst)))
            (mapcar 'car (postmodern:query (:select 'disease_id :from 'op_diag
                                            :where (:= 'op_id op-id)))))
    lst))

(defun op.year-is-p (op-id year)
  "Report true or nil whether the designated op-id was performed in the designated year"
	   (if (op-id-to-year op-id)
	       (= (op-id-to-year op-id) year)
	       nil))

(defun op.spine-op (year)
  "Report the number of spine surgery in the designated year"
	    (length (remove-if-not #'(lambda (x) (and (op.year-is-p x year) (op.is-spine-p x))) 
		    (mapcar 'car (postmodern::query (:select 'op-id :from 'op))))))

(defun op.is-spine-p (op-id)
  (member 2 (mapcar 'car (postmodern:query (:select 'd.major_div_id
                             :from (:as 'op 'o)
                             :left-join (:as 'op_diag 'od)
                             :on (:= 'o.op_id 'od.op_id)
                             :left-join (:as 'diagnosis 'd)
                             :on (:= 'od.disease_id 'd.disease_id)
                             :where (:= 'o.op_id op-id))))))

(defun op.is-brain-p (op-id)
  (member 1 (mapcar 'car (postmodern:query (:select 'd.major_div_id
                             :from (:as 'op 'o)
                             :left-join (:as 'op_diag 'od)
                             :on (:= 'o.op_id 'od.op_id)
                             :left-join (:as 'diagnosis 'd)
                             :on (:= 'od.disease_id 'd.disease_id)
                             :where (:= 'o.op_id op-id))))))

(defun op.get-op-date (op-id)
  "Obtain op_date in postmodern format from the designated op-id"
  (caar (postmodern::query (:select 'op_date :from 'op
                                   :where (:= 'op_id op-id)))))
(defun sort-oplist (op-id-lst)
  "Sort the op-id list with op-date"
	     (sort op-id-lst #'postmodern::time< :key #'op.get-op-date))

(defun pt.get-kanjiname (patient-id)
	   (caar (postmodern:query (:select 'kanji_name
					   :from 'patient
					   :where (:= 'patient_id patient-id)))))

(defun pt.get-opid-lst (patient-id)
  "Returns the list of op_id associated with the given patient_id"
  (mapcar 'car (postmodern:query (:order-by
                                  (:select 'op_id 'op_date
                                   :from (:as 'op 'o)
                                   :left-join (:as 'patient 'p)
                                   :on (:= 'o.patient_id 'p.patient_id)
                                   :where (:= 'p.patient_id patient-id))
                                  'op_date))))

(defun pt.disease-id-lst (patient-id)
  (let ((lst))
    (mapcar #'(lambda (x) (setf lst (adjoin x lst)))
            (mapcar 'car (postmodern:query (:select 'disease_id :from 'pt_diag
                                            :where (:= 'patient_id patient-id)))))
    lst))

(defun pt.diag-op-lst (patient-id)
	   (list (pt.get-kanjiname patient-id) patient-id
		 (mapcar #'(lambda (x)
                             (list x (diag.disid-to-disname x))) (pt.disease-id-lst patient-id))
		 (mapcar #'(lambda (x)
			     (list x (op.get-preopdx x))) (pt.get-opid-lst patient-id))))

(defun diag.disid-to-disname (disease-id)
	   (caar (postmodern:query (:select 'disease_name
				      :from 'diagnosis
				      :where (:= 'disease_id disease-id)))))
;;;
;;;
(defun dis-id.get-majordiv (dis-id)
	   (caar (postmodern:query (:select 'm.major_div
			    :from (:as 'diagnosis 'd)
			    :left-join (:as 'majordiv 'm)
			    :on (:= 'd.major_div_id 'm.major_div_id)
			    :where (:= 'disease_id dis-id)))))

(defun set-majordiv (dis-id)
	   (postmodern:query (:update 'diagnosis
				      :set 'major_div
				      (dis-id.get-majordiv dis-id)
				      :where (:= 'disease_id dis-id))))

(defun dis-id.get-pathodiv (dis-id)
	   (caar (postmodern:query (:select 'p.patho_div
			    :from (:as 'diagnosis 'd)
			    :left-join (:as 'pathodiv 'p)
			    :on (:= 'd.patho_div_id 'p.patho_div_id)
			    :where (:= 'disease_id dis-id)))))

(defun set-pathodiv (dis-id)
	   (postmodern:query (:update 'diagnosis
				      :set 'patho_div
				      (dis-id.get-pathodiv dis-id)
				      :where (:= 'disease_id dis-id))))

(defun dis-id.get-location (dis-id)
	   (caar (postmodern:query (:select 'l.location
			    :from (:as 'diagnosis 'd)
			    :left-join (:as 'location 'l)
			    :on (:= 'd.location_id 'l.location_id)
			    :where (:= 'disease_id dis-id)))))

(defun set-location (dis-id)
	   (postmodern:query (:update 'diagnosis
				      :set 'location
				      (dis-id.get-location dis-id)
				      :where (:= 'disease_id dis-id))))

(defmacro fill-diagnosis (lst)
  "Fill the fields of majordiv, pathodiv, location
 in the diagnosis of the disease_id in the lst"
	   `(mapcar #'(lambda (x)
			(set-majordiv x)
			(set-pathodiv x)
			(set-location x))
		    ,lst))



(defun op-list (disease-id)
  "Return the list of op_id associated with the given disease-id"
	   (mapcar 'car (postmodern::query
                         (:order-by
                          (:select 'o.op_id 'o.op_date :from (:as 'op 'o)
                                                       :left-join (:as 'op_diag 'od)
                                                       :on (:= 'o.op_id 'od.op_id)
                                                       :where (:= 'od.disease_id disease-id))
                          'o.op_date))))

(defun disease-name-query (disease-name)
  "Get the list of op_id from the disease-name"
  (mapcar 'car (postmodern:query (:order-by (:select
                                             'o.op-id
                                             :from (:as 'op 'o)
                                             :left-join (:as 'op-diag 'od)
                                             :on (:= 'o.op_id 'od.op_id)
                                             :left-join (:as 'diagnosis 'd)
                                             :on (:= 'od.disease_id 'd.disease_id)
                                             :left-join (:as 'disease_name 'dn)
                                             :on (:= 'd.disease_name_id 'dn.disease_name_id)
                                             :where (:= 'dn.disease_name disease-name))
                                            'o.op_date))))

(defun op.get-start-time (op-id) 
	   (let ((result (postmodern:query (:select
			    'start-time
			    :from 'op
			    :where (:= 'op_id op-id)))))
             (if (and (not (equal result :null)) result)
                 (caar result)
                 nil)))

(defun op.get-end-time (op-id)
  (let ((result (postmodern:query (:select
                                   'end-time
                                   :from 'op
                                   :where (:= 'op_id op-id)))))
    (if (and (not (equal result :null)) result)
        (caar result)
        nil)))

(defun encode-timestring (timestring)
  "Encode the timestring hh:mm:ss to number of minutes"
	   (let ((vec (multiple-value-bind (string vector)
	     (cl-ppcre:scan-to-strings "\([0-9]{2}\):\([0-9]{2}\):\([0-9]{2}\)" timestring)
		     vector)))
	   (if vec
	       (+ (* (parse-integer (elt vec 0)) 60) (parse-integer (elt vec 1)))
	       nil)))

(defun op.get-op-time (op-id)
  "Get the operation time in minutes"
	   (let ((start-time (encode-timestring (op.get-start-time op-id)))
		 (end-time (encode-timestring (op.get-end-time op-id))))
	     (if (and start-time end-time)
		 (if (< start-time end-time)
		     (- end-time start-time)
		     (+ (- (* 24 60) start-time) end-time)))))