(require 'cl-ppcre)
(require 'postmodern)

(defun output-parbox (zip-code address kanji-name)
  (format nil "\\parbox[c][5.08cm][c]{8.64cm}{~A\\\\~A\\\\~A}"
          zip-code address kanji-name))

(defun output-parbox-from-id (op-id)
  "This function returns the TeX parbox suited for the address label"
  (let ((op (make-instance-of-op-from-id op-id)))
    (output-parbox (zip-code op) (address op) (kanji-name op))))

(defun pid-2-oid (pid)
  "This function in lumbar_study.lisp takes a patient-id and returns the oldest op-id of that patient"
  (let ((op-list (mapcar #'first (postmodern:query (:select 'op_id :from 'op
					    :where
					    (:= 'patient_id pid))))))
    (cond ((endp op-list) nil)
	  (t (apply #'min op-list)))))

;;Class for the case_series data
(defclass patient ()
  ((kanji-name :accessor kanji-name
               :initform nil
               :initarg :kanji-name)
   (zip-code :accessor zip-code
             :initform nil
             :initarg :zip-code)
   (address :accessor address
            :initform nil
            :initarg :address)
   (sf-0  :accessor sf-0
          :initform nil
          :initarg :sf-0)
   (sf-6 :accessor sf-6
         :initform nil
         :initarg :sf-6)
   (sf-12 :accessor sf-12
          :initform nil
          :initarg :sf-12)
   (sf-24 :accessor sf-24
          :initform nil
          :initarg :sf-24)
   (sf-36 :accessor sf-36
          :initform nil
          :initarg :sf-36)
   (sf-48 :accessor sf-48
          :initform nil
          :initarg :sf-48)
   (sf-60 :accessor sf-60
          :initform nil
          :initarg :sf-60)
   (ps-0 :accessor ps-0
         :initform nil
         :initarg :ps-0)
   (ps-6 :accessor ps-6
         :initform nil
         :initarg :ps-6)
   (ps-12 :accessor ps-12
         :initform nil
         :initarg :ps-12)
   (ps-24 :accessor ps-24
         :initform nil
         :initarg :ps-24)
   (ps-36 :accessor ps-36
         :initform nil
         :initarg :ps-36)
   (ps-48 :accessor ps-48
         :initform nil
         :initarg :ps-48)
   (ps-60 :accessor ps-60
         :initform nil
         :initarg :ps-60)
   (bp-0 :accessor bp-0
         :initform nil
         :initarg :bp-0)
   (bp-6 :accessor bp-6
         :initform nil
         :initarg :bp-6)
   (bp-12 :accessor bp-12
         :initform nil
         :initarg :bp-12)
   (bp-24 :accessor bp-24
         :initform nil
         :initarg :bp-24)
   (bp-36 :accessor bp-36
         :initform nil
         :initarg :bp-36)
   (bp-48 :accessor bp-48
         :initform nil
         :initarg :bp-48)
   (bp-60 :accessor bp-60
         :initform nil
         :initarg :bp-60)
   ))

(defmethod print-object ((p patient) stream)
  (format stream "~A, ~A, ~A~%" (kanji-name p) (zip-code p) (address p)))

(defun make-patient (kanji-name zip-code address)
  (make-instance 'patient :kanji-name kanji-name
                 :zip-code zip-code
                 :address address))

(defun insert-patient (result kanji-name zip-code address)
  (push (make-patient kanji-name zip-code address)
        result))

(defun obtain-property (property-string line)
  "Return the data of the property contained in line"
  (let ((regular-exp
         (format nil "~A: (.*)" property-string))) 
    (multiple-value-bind (mstr reglist)
        (cl-ppcre:scan-to-strings regular-exp line)
      (if (null reglist)
          nil
          (elt reglist 0)))))

(defun replace-null-to-nil (str)
  (if (eq str :NULL)
      nil
      str))

(defun data-from-pid (patient-id)
  (let ((result (postmodern:query (:select 'kanji_name 'zipcode 'address
                             :from 'patient
                             :where
                             (:= 'patient_id patient-id)))))
    (if (not (null result))
        (values (replace-null-to-nil (first (first result)))
                (replace-null-to-nil (second (first result)))
                (replace-null-to-nil (third (first result))))
        (values nil nil nil))))

(defun process-case-file (filename)
  (let ((start-flag)
        (result)
        (patient-id)
        (kanji-name)
        (zip-code)
        (address))
    (with-open-file (str filename)
      (do ((line (read-line str nil 'eof) (setf line (read-line str nil 'eof))))
          ((eq line 'eof) (push (make-patient kanji-name zip-code address) result))
        (cond
          ((cl-ppcre:scan "^\\* " line)
           (if (not (null start-flag))
               (progn
                 (push (make-patient kanji-name zip-code address) result)
                 (setf patient-id nil kanji-name nil zip-code nil address nil)
                 (setf start-flag t))
               (setf start-flag t)))
          (t
           (setf patient-id (obtain-property "patient_id" line))
           (if (not (null patient-id))
               (progn
                 (multiple-value-bind (kn zc ad) (data-from-pid patient-id)
                   (setf kanji-name kn zip-code zc address ad)
                   (setf start-flag t))))))))
    (reverse result)))

               



