(require 'postmodern)
(require 'cl-ppcre)

(import 'postmodern:connect-toplevel)


(defparameter *disease-name-list* (postmodern:query (:select 'disease_name_id 'disease_name :from 'disease_name)))

(defparameter *foraminotomy-list* '(00214635 99107294 89103386 06033351 04082723 05070347 05183066 97171342 03218526 06192504
                   06230668 06204226 06173389 06121586 06005433 94017092))

;(defun disease-name-query (string)
;  (let ((result nil))
;    (mapcar #'(lambda (x) (if (cl-ppcre:scan string (second x))
;                              (setf result (append result (list x))))) *disease-name-list*)
;    result))


(defun op-with-disease-name (name)
  "Query the op-id associated with the disease-name through the op_diag table"
	   (mapcar 'car
		   (postmodern:query (:select 'op.op_id
				      :from 'op
				      :left-join (:as 'op_diag 'od)
				      :on (:= 'op.op_id 'od.op_id)
				      :left-join (:as 'diagnosis 'd)
				      :on (:= 'od.disease_id 'd.disease_id)
				      :left-join (:as 'disease_name 'dn)
				      :on (:= 'd.disease_name_id 'dn.disease_name_id)
				      :where (:= 'd.disease_name name)))))

(defun op-with-disease-name-id (id)
  "Return the list of op_id querying the op_diag table for disease_name_id"
	   (postmodern:query (:select 'op.op_id
				      :from 'op
				      :left-join (:as 'op_diag 'od)
				      :on (:= 'op.op_id 'od.op_id)
				      :left-join (:as 'diagnosis 'd)
				      :on (:= 'od.disease_id 'd.disease_id)
				      :where (:= 'd.disease_name_id id))))

;;;Query the database with disease-name-id
;;;   The disease-name-id can be obtained referring to the disease-name-list variable
;;;   reports patient_id, kanji_name, op_id
(defun patient-with-disease-name (id)
  "Query the patient database from disease_name_id"
  (postmodern:query (:select 'p.patient_id 'p.kanji_name 'op.op_id :from (:as 'patient 'p) :left-join 'op :on (:= 'p.patient_id 'op.patient_id) :left-join (:as 'pt_diag 'pd) :on (:= 'pd.patient_id 'p.patient_id) :left-join (:as 'diagnosis 'd) :on (:= 'd.disease_id 'pd.disease_id) :where (:= 'd.disease_name_id id))))

(defun disease-name-id-image-query (id)
  "Query to search for patients with disease-name-id who has image data in the database."
  (postmodern:query (:select 'p.patient_id 'p.kanji_name 'i.image
                              :from (:as 'patient 'p)
                              :left-join (:as 'images 'i) :on (:= 'p.patient_id 'i.patient_id)
                              :left-join (:as 'pt_diag 'pd) :on (:= 'pd.patient_id 'p.patient_id)
                              :left-join (:as 'diagnosis 'd) :on (:= 'd.disease_id 'pd.disease_id)
                              :where (:and (:= 'd.disease_name_id id) (:not (:is-null 'image))))))

(defun patient-name-query (name)
  "Query the patient database from part of patient-name"
  (postmodern:query (:select '* :from 'patient
                              :where (:like 'kanji_name
                                            (concatenate 'string "%" name "%")))))
(defun sf36-query (id)
  "Query the sf-36 database with patient_id"
  (postmodern:query (:select '* :from 'sf36
                              :where (:= 'patient_id (prep-id id)))))


(defun prep-id (id)
  (if (stringp id)
      id
      (if (> id 10000000)      ;七桁以下なら、最初に０を足す必要あり
          (write-to-string id)
          (if (> id 1000000)
              (concatenate 'string
                           "0"
                           (write-to-string id))
              (concatenate 'string
                           "00"
                           (write-to-string id))))))

(defun patient-id-query (id)
  (postmodern:query (:order-by
                     (:select 'o.op_id 'p.patient_id 'p.kanji_name
                              'o.op_date 'o.preop_dx 'o.procedure 'o.indication 'o.op_note
                              :from (:as 'patient 'p)
                              :left-join (:as 'op 'o)
                              :on (:= 'p.patient_id 'o.patient_id)
                              :where
                              (:= 'p.patient_id (prep-id id)))
                     'o.op_date)))

;;;
;;;Query the op.op_note and op.indication for the word (arg)
;;;           return patient_id, kanji_name, operation date, procedure
(defun op-note-query (arg)
  "Query the database for arg."
  (postmodern:query (:select 'p.patient_id 'p.kanji_name 'o.op_id 'o.procedure 'o.op_date
                              :from (:as 'patient 'p)
                              :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
                              :where (:or
                                      (:like 'o.op_note (concatenate 'string "%" arg "%"))
                                      (:like 'o.indication (concatenate 'string "%" arg "%"))))))

(defun op-id-query (id)
  (car
   (postmodern:query (:select 'o.op_id 'p.patient_id 'p.kanji_name 'o.op_date
                              'o.start_time 'o.end_time 'o.preop_dx 'o.indication
                              'o.procedure 'o.op_note 'o.surgeons 'o.assistants
                              :from (:as 'patient 'p)
                              :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
                              :where (:= 'o.op_id id)))))

(defun describe-op (id &key tabulate (file t) (version :short))
   (multiple-value-bind (op-id patient-id kanji-name op-date preop-dx procedure indication opnote)
       (values-list (car
                     (postmodern::query (:order-by
                                         (:select 'o.op_id 'p.patient_id 'p.kanji_name 'o.op_date
                               'o.preop_dx 'o.procedure 'o.indication 'o.op_note
                               :from (:as 'patient 'p)
                               :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
                               :where (:= 'o.op_id id))
                       'o.op_date))))
     (if (not tabulate)
         (cond
           ((equal version :long)
            (format file "~A~%~A~%~A~%~A ~A~%~A~%~A~%~A~%~A~%~%"
                    op-id (my-decode-date op-date) patient-id
                 kanji-name (age-from-op id) preop-dx procedure indication opnote))
           ((equal version :medium)
            (format file "~A~%~A~%~A~%~A ~A~%~A~%~A~%~A~%~%"
                    op-id (my-decode-date op-date) patient-id
                 kanji-name (age-from-op id) preop-dx procedure indication))
           (t
            (format file "~A~%~A~%~A~%~A ~A~%~A~%~A~%~%"
                    op-id (my-decode-date op-date) patient-id
                 kanji-name (age-from-op id) preop-dx procedure)))
         (format file "~A~C~A~C~A~C~A~C~A~C~A~%"
                 op-id #\tab
                 (my-decode-date op-date) #\tab
                 patient-id #\tab
                 kanji-name #\tab
                 (age-from-op id) #\tab
                 preop-dx procedure))))



;;;print-op-note-query
;;;Search the op.op_note and op.indication for the keyword, and print the results
(defun print-op-note-query (word)
  (pretty-print (mapcar #'(lambda (x) (op-id-query x))
                        (mapcan #'(lambda (x) (list (third x)))
                                (op-note-query word)))))

(defun print-item (lst)
  "print each item of the lst
if lst is not a cons, then just returns lst"
  (if (not (consp lst))
      (format t "~A~%" lst)
      (dolist (item lst (format t "~%~%"))
        (format t "~A   " item))))


(defun csv-output (stream lst)
  (labels ((tab-out (x) (if (null x)
                            (format stream "~%")
                            (progn
                              (format stream "~A~C" (car x) #\tab)
                              (tab-out (cdr x))))))
    (if (null lst)
        (return-from csv-output)
        (progn
          (tab-out (car lst))
          (csv-output stream (cdr lst))))))
		
;;;Print the pertinent information from the list of patient_id's
;;;
(defun print-id-list (lst)
  "Print the data of the patient_id list lst"
	   (mapcar #'(lambda (x) (print-item (car x)))
		   (mapcar #'(lambda (x) (patient-id-query x)) lst)))

(defun my-decode-date (date)
  (if (not (equal date :null))
      (multiple-value-bind (sec min h day month year)
          (decode-universal-time date)
        (format nil "~A-~A-~A" month day year))
      nil))

(defun date-to-month (date)
  (multiple-value-bind (year month day)
      (simple-date:decode-date date)
    month))


(defun date-to-year (date)
  (if (not (equal date :null))
      (multiple-value-bind (year month day)
          (simple-date:decode-date date)
        year)
      nil))

(defun op-id-to-year (op-id)
  "Report the year of the operation: op-id"
	   (date-to-year (caar (postmodern:query
				(:select 'op_date :from 'op
					 :where (:= 'op_id op-id))))))

;;;Output the formatted latex file from the op_id
;;;    The output file is /home/chang/opnote/opnote-$OP_ID.tex
;;;
(defun format-op (id)
  (multiple-value-bind (op-id patient-id patient-name date start-time end-time preop-dx
				     indication procedure op-note surgeons assistants)
	     (values-list (op-id-query id))
    (let ((filename (format nil "/home/chang/opnote/opnote-~A.tex" op-id)))
      (with-open-file (stream filename
                              :direction :io
                              :if-exists :overwrite
                              :if-does-not-exist :create)
        (format stream "\\documentclass[a4paper, 12pt]{jarticle}
\\setlength{\\topmargin}{-0.5cm}
\\setlength{\\topskip}{-0.5cm}
\\setlength{\\headheight}{0cm}
\\setlength{\\footskip}{1cm}
\\setlength{\\oddsidemargin}{0cm}
\\setlength{\\evensidemargin}{0cm}
\\setlength{\\textheight}{25cm}
\\setlength{\\textwidth}{17cm}
\\renewcommand{\\rmdefault}{ugm}
\\usepackage[T1]{fontenc}
\\usepackage{textcomp}
\\begin{document}
\\begin{center}
{\\bfseries \\large Operation Record\\\\}
\\end{center}
{\\bfseries Patient ID: }~A\\\\
{\\bfseries Patient Name: }~A\\\\
{\\bfseries Operation Date: }~A\\\\
{\\bfseries Surgeons: }~A\\\\
{\\bfseries Assistants: }~A\\\\
{\\bfseries Starting time: }~A\\\\
{\\bfseries Finishing time: }~A\\\\
{\\bfseries Preop Diagnosis: }~A\\\\\\\\
{\\bfseries Procedure: }~A\\\\\\\\
{\\bfseries Indication:}~%~A~%\\\\\\\\
{\\bfseries Procedure:}~%~A
\\end{document}"
                patient-id
                patient-name
                (my-decode-date date)
                surgeons
                assistants
                start-time
                end-time
                preop-dx
                procedure
                indication
                op-note)))))


(defun timing-stat (timing-lst)
	   (let ((result (make-array 5))
		 (timevec (make-array 5 :initial-contents '(0 6 12 24 36)))
		 (location))
	     (mapcar #'(lambda (timing)
			 (if (setf location (position timing timevec))
			     (incf (aref result location))))
		     timing-lst)
	     result))

;;;
;;;List of the patient_id for whom only one surgery was performed
;;;Thus, we can set cold = true for the operations of the patients in this list
;;;
(defparameter *automatic-set* '("05700109" "05700166" "06035604" "07008733" "03180932" "06186308" "06192819"
 "06166797" "06179899" "06152235" "06119267" "06173389" "06170708" "06175442"
 "01111939" "06142053" "06124077" "06162051" "94123585" "06163539" "99103681"
 "97171342" "96054192" "94096526" "06124515" "06119622" "06098016" "06088900"
 "06081699" "06081467" "06051932" "06048409" "05217948" "05124623" "05093232"
 "04101408" "03219219" "03214541" "03146800" "03131919" "03109907" "06109912"
 "06121586" "06114292" "04087656" "06068308" "06029870" "79097598" "06012116"
 "06005433" "05167424" "05070347" "04082723" "04105870" "04091971" "04046686"
 "03218526" "03195872" "03187267" "03184777" "03098761" "03065570" "03009206"
 "06052591" "06137343"))

;;;
;;;List of the op_id of the cold research
;;;
;(defparameter *cold-op* '(533 529 556 712 740 738 644 731 729 718 717 716 715 714 713 712 710 711 707 704 703 701 702 699 555 606 611 628 693 686 673 677 670 667 664 648 635 597 588 563 503 504 501 694 383 698 690 683 680 659 639 638 636 632 612 581 539 529 528 512 500 498 494 464 380 369 368 696 695))

(defparameter *cold-op* (mapcar 'car (postmodern:query (:select 'op-id :from 'op :where (:= 'coldp "true")))))

(defun mark-cold (op-id)
  "Set coldp = true of the record op-id"
  (postmodern:query (:update 'op :set 'coldp 'true :where (:= 'op_id op-id))))


(defun timing-to-list (lst)
  (let ((pos)
        (result (list 0 0 0 0 0))
        (timing-vec '(0 6 12 24 36)))
    (if (null lst)
        (return-from timing-to-list result))
    (dolist (x lst)
      (if (setf pos (position x timing-vec))
          (incf (nth pos result))))
    result))

(defun get-timing (pt-id)
    (mapcar #'car
            (postmodern:query (:select 'timing :from 'sf36 :where (:= 'patient_id pt-id)))))

(defun get-month-data (pt-data)
  (date-to-month (third pt-data)))

;;Special comparison function used for the function get-patient-list
;;    First comparison: month
;;    Second comparison: year
(defun compare-date (date1 date2)
  (let ((month1 (date-to-month date1))
        (month2 (date-to-month date2)))
    (if (= month1 month2)
      (< (date-to-year date1) (date-to-year date2))
      (< month1 month2))))

;;get-patient-list
;;  output a list of the patients with cold sorted by the month of the operation
(defun get-patient-list ()
  (let ((datalist))
    (setf datalist (postmodern:query (:select 'p.patient_id 'p.kanji_name 'o.op_date 'o.preop_dx
                                              :from (:as 'patient 'p)
                                              :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
                                              :where (:= 'o.coldp 'true))))
    (setf datalist (mapcar #'(lambda (x) (append x (timing-to-list (get-timing (car x))))) datalist))
    (mapcar
     #'(lambda (x) (setf (third x) (my-decode-date (third x)))
               x)
     (sort datalist #'(lambda (x y) (compare-date (third x) (third y)))))))


(defun get-address (id)
  (car (postmodern:query (:select 'kanji_name 'address
                             :from 'patient
                           :where (:= 'patient_id id)))))

(defun address-list (list)
  (mapcar #'(lambda (x) (get-address x)) list))

;;Obtain the list of cold patients whose op-date matches the value specified
;;by the arguments months and optional year
(defun patient-list-from-opdate (month &optional year)
	   (let ((result)
		 (str (concatenate 'string
				   "^"
				   (write-to-string month)
				   "-.*-"
				   (if year (write-to-string year)))))
	     (dolist (x (get-patient-list))
	       (if (cl-ppcre:scan str (third x))
		   (push x result)))
	     result))

;;Decode date string "mm-dd-yyyy" into integer value of month
(defun date-string-to-month (string)
	   (let ((result
		  (parse-integer
                   (aref
                    (second (multiple-value-list
                             (cl-ppcre:scan-to-strings
                              "([0-9]*)-([0-9]*)-([0-9]*)" string))) 0))))
	     result))

;;Decode date string "mm-dd-yyyy" into integer value of year
(defun date-string-to-year (string)
	   (let ((result
		  (parse-integer
                   (aref
                    (second (multiple-value-list
                             (cl-ppcre:scan-to-strings
                              "([0-9]*)-([0-9]*)-([0-9]*)" string))) 2))))
	     result))

;;Decode date string "mm-dd-yyyy" into integer value of year
(defun date-string-to-day (string)
	   (let ((result
		  (parse-integer
                   (aref
                    (second (multiple-value-list
                             (cl-ppcre:scan-to-strings
                              "([0-9]*)-([0-9]*)-([0-9]*)" string))) 1))))
	     result))



;;; 日付関係の関数

;;Encode the date string of the form "mm-dd-yyyy" into
;;the time stamp
(defun encode-date-string (string)
  (multiple-value-bind
        (s dset)
      (cl-ppcre:scan-to-strings "([0-9]*)-([0-9]*)-([0-9]*)" string)
    (let ((month (parse-integer (elt dset 0)))
          (day (parse-integer (elt dset 1)))
          (year (parse-integer (elt dset 2))))
      (encode-universal-time 0 0 0 day month year))))

(defun encode-date (day month year)
	   (encode-universal-time 0 0 0 day month year))

;;evaluatedは、xより６ヶ月前と判断する関数
(defun six-month-ago-p (current evaluated)
  (if (and
       (< (- current (* 30 24 60 60 7)) evaluated)
       (> (- current (* 30 24 60 60 5)) evaluated))
      t
      nil))

;;EVALUATEDは、CURRENTより１年前であると判断する関数
(defun one-year-ago-p (current evaluated)
  (if (and
       (< (- current (* 30 24 60 60 13)) evaluated)
       (> (- current (* 30 24 60 60 11)) evaluated))
      t
      nil))

;;EVALUATEDは、CURRENTより２年前であると判断する関数
(defun two-year-ago-p (current evaluated)
  (if (and
       (< (- current (* 30 24 60 60 27)) evaluated)
       (> (- current (* 30 24 60 60 21)) evaluated))
      t
      nil))

(defun three-year-ago-p (current evaluated)
  (if (and
       (< (- current (* 30 24 60 60 39)) evaluated)
       (> (- current (* 30 24 60 60 33)) evaluated))
      t
      nil))



;;current-date-stringより、６ヶ月前の患者リストをpt-listから抽出する関数
(defun six-month-list (curr-date-string pt-list)
  (let ((temp-result)
        (final-result)
        (current-encdate (encode-date-string curr-date-string)))
    (dolist (x pt-list)
      (if (six-month-ago-p current-encdate (encode-date-string (third x)))
          (push x temp-result)))
    (dolist (x temp-result)
      (if (= (sixth x) 0)
          (push x final-result)))
    final-result))

(defun one-year-list (curr-date-string pt-list)
  (let ((temp-result)
        (final-result)
        (current-encdate (encode-date-string curr-date-string)))
    (dolist (x pt-list)
      (if (one-year-ago-p current-encdate (encode-date-string (third x)))
          (push x temp-result)))
    (dolist (x temp-result)
      (if (= (seventh x) 0)
          (push x final-result)))
    final-result))

(defun two-year-list (curr-date-string pt-list)
  (let ((temp-result)
        (final-result)
        (current-encdate (encode-date-string curr-date-string)))
    (dolist (x pt-list)
      (if (two-year-ago-p current-encdate (encode-date-string (third x)))
          (push x temp-result)))
    (dolist (x temp-result)
      (if (= (eighth x) 0)
          (push x final-result)))
    final-result))

(defun three-year-list (curr-date-string pt-list)
  (let ((temp-result)
        (final-result)
        (current-encdate (encode-date-string curr-date-string)))
    (dolist (x pt-list)
      (if (three-year-ago-p current-encdate (encode-date-string (third x)))
          (push x temp-result)))
    (dolist (x temp-result)
      (if (= (ninth x) 0)
          (push x final-result)))
    final-result))

;;;Questionnaireを送付するべきリストを返す関数
(defun question-list (date-string)
  (let ((pt-list (get-patient-list)))
    (sort (concatenate 'list
                 (six-month-list date-string pt-list)
                 (one-year-list date-string pt-list)
                 (two-year-list date-string pt-list)
                 (three-year-list date-string pt-list))
          (lambda (x y) (< (encode-date-string (third x))
                           (encode-date-string (third y)))))))

;;;Get the list of address and name of the patients
;;;to whome we need to send the questionnaire based on the date
(defun get-question-address (date)
	   (mapcar #'(lambda (id)
		       (car (postmodern:query (:select
			    'address 'kanji_name :from 'patient
			    :where (:= 'patient_id id)))))
		   (mapcar 'car (question-list date))))

;;;Print the address and name of the patients to whome we need to send
;;;questionaire based on the specified date.  Output to the specified file
(defun print-question-list (filename date)
	   (with-open-file
	       (stream filename
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :overwrite)
	     (csv-output stream (get-question-address date))))

(defun patient-profile (id)
  (car
   (postmodern:query
   (:select 'kanji_name 'address 
            :from (:as 'patient 'p)
            :where (:= 'p.patient_id (prep-id id))))))

(defun output-qaddress (stream datestring)
  (csv-output stream (mapcar #'(lambda (x) (patient-profile (car x)))
		 (question-list datestring))))

;;;patientlistの住所氏名を整形した形でstreamに出力する関数
(defun taboutput-patientlist (stream patientlist)
  (csv-output stream (mapcar #'(lambda (x) (patient-profile (car x))) patientlist)))

;;;listを、繰り返しなしで、concatenateするユーティリティ
(defun concat-list-norep (&rest lists)
  (let ((final-list))
    (dolist (a-list lists)
      (dolist (item a-list)
        (pushnew item final-list :test 'equal)))
    final-list))

;;;２００７年１２月時点で溜まっていたquestioinnaire送付先のpatientlistを出力する関数
(defun question-list-this-time ()
  (sort (concat-list-norep
                     (question-list "12-11-2007")
                     (question-list "11-11-2007")
                     (question-list "10-11-2007")
                     (question-list "9-11-2007")
                     (question-list "8-11-2007")
                     (question-list "7-11-2007"))
        (lambda (x y) (< (encode-date-string (third x))
                         (encode-date-string (third y))))))



(defmacro valid-data (data)
  `(and (not (null ,data)) (not (equal ,data ""))))

;;;Test function　Keywordで、Fileから読み込む
(defun pt-file-process (filename)
  (let ((id) (name) (kananame) (birth) (sex)
        (address) (phone1) (phone2)
        (opdate) (starttime) (endtime) (procedure) (surgeons) (assistants)
        (emergency) (preopdx) (postopdx) (history) (opnote) (opid) (flag)) 
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line)
           (if (id-existp id)
               (print "ID already registered. Skip insertion of patient record")
               (progn
                 (setq flag (insert-pt id name kananame birth sex address phone1 phone2))
                 (if flag
                     (print "Successfully inserted one patient")
                     (print "Failed to insert a patient"))))
           (if (get-op-id opdate starttime)
               (return "Op already exists! Failed to insert an op record")
               (progn
                 (setq opid
                       (insert-op id (to-west-date opdate) starttime endtime procedure
                                  surgeons assistants emergency
                                  preopdx postopdx history opnote))
                 (if opid
                     (progn
                       (print
                        (format nil "Successfully inserted one op record. Op-id:~A"
                                opid))
                       (insert-surgeons opid surgeons)
                       (if (valid-data assistants)
                           (insert-assistants opid assistants)))
                     (print "Failed to insert an op record")))))
        (let ((result))
          (cond 
            ((and (not id) (setq result (line-search "^ID:(.*)" line)))
             (setq id result))
            ((and (not name) (setq result (line-search "^Name:(.*)" line)))
             (setq name result))
            ((and (not kananame) (setq result (line-search "^Kana:(.*)" line)))
             (setq kananame result))
            ((and (not birth) (setq result (line-search "^Birthdate:(.*)" line)))
             (setq birth result))
            ((and (not sex) (setq result (line-search "^Sex:(.*)" line)))
             (setq sex result))
            ((and (not address) (setq result (line-search "^Address:(.*)" line)))
             (setq address result))
            ((and (not phone1) (setq result (line-search "^Phone1:(.*)" line)))
             (setq phone1 result))
            ((and (not phone2) (setq result (line-search "^Phone2:(.*)" line)))
             (setq phone2 result))
            ((and (not opdate) (setq result (line-search "^Op_date:(.*)" line)))
             (setq opdate result))
            ((setq result (line-search "^Start_time:(.*)" line))
             (setq starttime result))
            ((setq result (line-search "^End_time:(.*)" line))
             (setq endtime result))
            ((setq result (line-search "^Procedure:(.*)" line))
             (setq procedure result))
            ((setq result (line-search "^Emergency:(.*)" line))
             (setq emergency (if (equal result "nil") nil  t)))
            ((setq result (line-search "^Surgeons:(.*)" line))
             (setq surgeons result))
            ((setq result (line-search "^Assistants:(.*)" line))
             (setq assistants result))
            ((setq result (line-search "^Preop_dx:(.*)" line))
             (setq preopdx result))
            ((setq result (line-search "^Postop_dx:(.*)" line))
             (setq postopdx result))
            ((setq result (line-search "^History:(.*)" line))
             (setq history result))
            ((setq result (line-search "^Opnote:(.*)" line))
             (setq opnote result))))))))


(defun id-existp (id)
  (if (postmodern:query (:select 'patient_id
                                 :from 'patient
                                 :where (:= 'patient_id id)))
      t nil))

;;key で lineを検索し、keyがなければnilを、見つければ、keyを値として返す関数
(defun line-search (key line)
    (multiple-value-bind (result caught) (cl-ppcre:scan-to-strings key line)
      (if result
          (elt caught 0)
          nil)))
    
(defun insert-pt (id name kananame birth sex address phone1 phone2)
  (let ((null-list)
        (phonelist))
    (unless (valid-data id) (setq null-list (cons "id" null-list)))
    (unless (valid-data name) (setq null-list (cons "name" null-list)))
    (unless (valid-data kananame) (setq null-list (cons "kananame" null-list)))
    (unless (valid-data birth) (setq null-list (cons "birth" null-list)))
    (unless (valid-data sex) (setq null-list (cons "sex" null-list)))
    (unless (valid-data address) (setq null-list (cons "address" null-list)))
    (if (valid-data phone1) (setq phonelist (cons phone1 phonelist)))
    (if (valid-data phone2) (setq phonelist (cons phone2 phonelist)))
    (if (not null-list)
        (progn
          (postmodern:query (:insert-into 'patient
                                          :set
                                          'patient_id id
                                          'kanji_name name
                                          'kana_name kananame
                                          'birthdate (to-west-date birth)
                                          'sex sex
                                          'address address))
          (if phonelist
              (dolist (x phonelist)
                (insert-phone id x)))
          id)
        (progn
          (print (format nil "Nil value in ~A~%" null-list))
          nil))))

(defun insert-phone (id phone)
  (postmodern:query (:insert-into 'phone
                                  :set
                                  'patient_id id
                                  'phone phone)))
                                  

(defun insert-op (id opdate starttime endtime procedure surgeons assistants
                  emergency preopdx postopdx history opnote)
  (let ((list))
    (unless (valid-data id) (setq list (cons "id" list)))
    (unless (valid-data opdate) (setq list (cons "opdate" list)))
    (unless (valid-data starttime) (setq list (cons "starttime" list)))
    (unless (valid-data endtime) (setq list (cons "endtime" list)))
    (unless (valid-data procedure) (setq list (cons "procedure" list)))
    (unless (valid-data surgeons) (setq list (cons "surgeons" list)))
    ;accept no data in assistants
    (unless (valid-data preopdx) (setq list (cons "preopdx" list)))
    (unless (valid-data postopdx) (setq list (cons "postopdx" list)))
    (unless (valid-data history) (setq list (cons "history" list)))
    (unless (valid-data opnote) (setq list (cons "opnote" list)))
    (if (not list)
        (progn
          (postmodern:query (:insert-into 'op
                                          :set
                                          'patient_id id
                                          'op_date opdate
                                          'start_time starttime
                                          'end_time endtime
                                          'procedure procedure
                                          'surgeons surgeons
                                          'assistants assistants
                                          'emergency emergency
                                          'preop_dx preopdx
                                          'postop_dx postopdx
                                          'indication history
                                          'op_note opnote))
          (get-op-id opdate starttime))
        (progn
          (print (format nil "Nil value in ~A" list))
          nil))))

(defun whitespace-split (string)
  (cl-ppcre:split "(\\s|　)*([,、]|(\\s|　))\\s*" string))


(defun insert-surgeons (op-id surgeon-string)
  (let ((surgeon-list (cl-ppcre:split
                       "(\\s|　)*([,、]|(\\s|　))\\s*" surgeon-string)))
    (dolist (x surgeon-list)
      (postmodern:query (:insert-into 'op_surgeon
                                      :set 'op_id op-id
                                      'surgeon_id (get-surgeon-id x))))))
(defun insert-assistants (op-id assistant-string)
  (let ((assistant-list (cl-ppcre:split
                         "(\\s|　)*([,、]|(\\s|　))\\s*" assistant-string)))
    (dolist (x assistant-list)
      (postmodern:query (:insert-into 'op_assistant
                                      :set 'op_id op-id
                                      'surgeon_id (get-surgeon-id x))))))
(defun get-surgeon-id (name)
  (caar (postmodern:query (:select 'surgeon_id
                           :from 'surgeons
                           :where (:= 'surgeon_name (strip-white-space name))))))

(defun strip-white-space (string)
  (multiple-value-bind (result list)
      (cl-ppcre:scan-to-strings "(\\s|　)*([^\\s　]*)(\\s|　)*" string)
    (if result
        (elt list 1))))

;;Old version
;;(defun strip-white-space (string)
;;  (multiple-value-bind (result list)
;;      (cl-ppcre:scan-to-strings "(\\s|　)*(.*[^ \t\n\r　])(\\s|　)*" string)
;;    (if result
;;        (elt list 1))))

(defun get-op-id (opdate starttime)
  (caar
   (postmodern:query (:select 'op_id
                              :from 'op
                              :where
                              (:and (:= 'op_date opdate) (:= 'start_time starttime))))))

(defun print-op-note (opid)
  "Print the op note of the op_id"
	   (let ((p-id) (k-name) (o-id) (preop-dx) (procedure) (indic) (opnote))
	     (mapcar #'(lambda (x) (print x))
		 (car (postmodern:query
		      (:select 'p.patient_id 'p.kanji_name 'o.op_id 'o.preop_dx 'o.procedure
			       'o.indication 'o.op_note
			       :from (:as 'patient 'p)
			       :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
						       :where (:= 'op_id opid)))))
	     nil))

(defun age-from-op (opid)
  "Return the age of the patient drawn from the indication of the op"
  (let ((indication
         (caar (postmodern:query (:select 'indication
                                     :from 'op
                                     :where (:= 'op_id opid))))))
    (if (or (not indication) (equal indication :null) (equal indication "")
            (not (cl-ppcre:scan-to-strings "This ([0-9]*)-t?year" indication)))
        nil
        (multiple-value-bind (p q)
            (parse-integer
             (multiple-value-bind (a b)
                 (cl-ppcre:scan-to-strings
                  "This ([0-9]*)-t?year" indication)
               (elt b 0)) :junk-allowed t)
          p))))


;;;average-age(op-lst)
;;;This function returns the average age of the patients in the given operation list
;;;in the form of the list of op_id.
;;;It obtains the patient's age from the first sentence of indication, which is
;;;"This **-year-old patient"
;;;    depends on age-from-op function
(defun average-age (op-lst)
  "Return the average age of the operation list"
  (float (statistics:mean (mapcar 'age-from-op op-lst))))

;;;calc-levels()
;;;Calculates the numbers of the lumbar levels in the list *cold-op*
;;;  returns the list of the numbers of (L2/3 L3/4 L4/5 L5/S1) respectively
(defun calc-levels ()
	   (let ((el-2 0) (el-3 0) (el-4 0) (el-5 0)
		 (levels
		  (let ((result))
		    (dolist (x *cold-op* result)
		      (setq result
			  (append (cl-ppcre:all-matches-as-strings "L[0-9]/S?[0-9]"
			    (caar (postmodern:query (:select 'procedure :from 'op
				    :where (:= 'op_id x))))) result))))))
	   (dolist (x levels (list el-2 el-3 el-4 el-5))
	     (cond
	       ((equal x "L2/3") (incf el-2))
	       ((equal x "L3/4") (incf el-3))
	       ((equal x "L4/5") (incf el-4))
	       ((equal x "L5/6") (incf el-5))
	       ((equal x "L5/S1") (incf el-5))))))
;;;bp-index (timing)
;;;  This function returns the list of bodily pain values
;;;  in those patients who 
(defun bp-index (timing &optional (listhesis nil opt))
  (cond ((null opt)
         (mapcar 'car (postmodern:query (:select 'bp :from 'sf36
					:where
					(:= 'timing timing)))))

         ((and opt listhesis)
          (mapcar 'car (postmodern:query (:select 'bp :from 'sf36
                                           :where
					(:and (:= 'timing timing)
					      (:in 'patient_id
						   (:select 'patient_id
				    :from 'patient
				    :where (:= 'listhesis 'true))))))))

        ((and opt (not listhesis))
         (mapcar 'car (postmodern:query (:select 'bp :from 'sf36
                                           :where
					(:and (:= 'timing timing)
					      (:not-in 'patient_id
						   (:select 'patient_id
				    :from 'patient
				    :where (:= 'listhesis 'true))))))))))


(defun bp-stat () (let ((result))
	   (dolist (x '(0 6 12 24 36) (reverse result))
	     (push (list (statistics:mean (bp-index x nil))
			 (statistics:standard-error-of-the-mean (bp-index x nil)))
		   result))))
(defun bp-stat-listplus () (let ((result))
	   (dolist (x '(0 6 12 24 36) (reverse result))
	     (push (list (statistics:mean (bp-index x t))
			 (statistics:standard-error-of-the-mean (bp-index x t)))
		   result))))
(defun bp-stat-listminus () (let ((result))
	   (dolist (x '(0 6 12 24 36) (reverse result))
	     (push (list (statistics:mean (bp-index x nil))
			 (statistics:standard-error-of-the-mean (bp-index x nil)))
		   result))))

;;;mean-follow-up-listhesis
;;; Function to calculate the mean follow up period in months
;;; of those patients who had cold operation and had preoperative sopndylolisthesis
(defun mean-follow-up-listhesis ()
  (float (statistics:mean (mapcar #'(lambda (x) (/
				(-
				 (encode-date-string "04-10-2008")
				 (encode-date-string (my-decode-date (car x))))
				(* 60 60 24 30)))
	    (postmodern:query
	  (:select 'o.op_date :from (:as 'op 'o)
		   :left-join (:as 'patient 'p)
		   :on (:= 'p.patient_id 'o.patient_id)
		   :where
		   (:and
		    (:= 'o.coldp 'true)
		    (:in 'p.patient_id (:select 'p.patient_id
			    :from (:as 'patient 'p)
			    :left-join (:as 'op 'o)
			    :on (:= 'p.patient_id 'o.patient_id)
			    :where
			    (:and
			     (:= 'o.coldp 'true)
			     (:= 'p.listhesis 'true)))))))))))

(defun mean-follow-up (mm-dd-yyyy op-lst)
  "Calculate mean follow up from the postgres-date-list and the date string mm-dd-yy"
  (let ((follow-up-lst (mapcar #'(lambda (x) (/
				(-
				 (encode-date-string mm-dd-yyyy)
				 (encode-date-string (my-decode-date x)))
				(* 60 60 24 30)))
                                  (mapcar 'op.get-op-date op-lst))))
    (values (float (statistics::mean follow-up-lst))
            (float (apply #'statistics::min follow-up-lst))
            (float (apply #'statistics::max follow-up-lst)))))


(defun print-op-output (x)
  "Print to standard output the output of the item of patient-id-query"
  (format t
          "Op-id: ~a~%Patient ID: ~a~%Patient Name: ~a~%Op date: ~a~%Preop Dx: ~a~%Procedure: ~a~%Indication: ~a~%Opnote: ~a~%~%"
          (first x) (second x) (third x)
          (let ((dresult (fourth x)))
            (if (and dresult (not (equal dresult :NULL)))
                (my-decode-date dresult)
                "no data"))
          (fifth x) (sixth x) (seventh x) (eighth x)))