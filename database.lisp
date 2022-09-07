;;;;
;;;;Functions to query the database "patient" in localhost
;;;;   especially for sending questionnaires to the established list of patients
;;;;

(require 'clsql)

;;Establish connection to th database specified by the hostname and the dbname
(defun db-connect (host dbname)
  (clsql:connect
   (list host dbname "chang" "nfkc505")
   :database-type :postgresql))
(defvar plist ())
(defun add-plist ()
  (let (id)
    (loop
     (if (equal "q" (setq id (read-line)))
         (return))
     (setq plist (cons id plist)))))

;;Current list of the patient to whome we need to send the questionnaire
(setf *current-list*
  '("06192819" "06179899" "06166797" "06173389" "06119267" "06152235" "06170708"
   "01111939" "06142053" "94123585" "06124077" "06119622" "06114292" "06098016"
   "06081699" "06081467" "06051932" "06068308" "06035604" "79097598" "94096526"
   "05070347" "05017413" "04091971" "04046686" "03219219" "03146800" "03187267"))

;;Query the database for address and kanji_name with the given patient_id
(defun query-address (pid)
  (let (sql)
    (setq sql "select address, kanji_name from patient where patient_id = '")
    (setq sql (concatenate 'string sql pid "'"))
  (clsql:query sql)))

;;Helper function to print address, kanji_name given the query result
(defun print-address (stream result)
	   (format stream "~{~a~^,~}" (car result)))
(defun final-print (stream)
  (mapcar #'(lambda (x)
              (print-address stream (query-address x))
              (format stream "~%"))
          current-list))