(require 'simple-date)
(require 'postmodern)

(in-package :lumbar-study)

;;;load-filtered-list: string -> void
;;;This function loads the saved op-list of the lumbar outcome study (/Users/chang/Documents/Lumbar_outcome/lumbar_list)
;;;into the global variable *op-list*
(defun load-filtered-list (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (mapcar #'make-instance-of-op-from-id (read in)))))

;１０４名の、lumbar outcome study対象患者の、operation instanceのリスト
(defvar *oplist* (load-filtered-list "/Users/chang/Documents/Lumbar_outcome/lumbar_list"))

;１０４名の、lumbar outcome study対象患者の、op_id のリスト
(defvar *oid-list* (mapcar #'op-id *oplist*))


;;sf-record
;;sf-recordとは、op-id, month (number), ps (number), ms (number), bp (number)
;;のフィールドを持つstructureで、
;;        op-id  op_id
;;        month: 手術後の月数(0, 6, 12, 24, 36, 48, etc)
;;        ps:  mean physical score
;;        ms:  mean mental score
;;        bp:  bodily pain score
(defstruct sf-record
  op-id
  month
  ps
  ms
  bp)

(defmethod print-object ((sfr sf-record) stream)
  (format stream "op-id: ~A month: ~A ps: ~A ms: ~A bp: ~A~%"
          (sf-record-op-id sfr)
          (sf-record-month sfr) (sf-record-ps sfr) (sf-record-ms sfr) (sf-record-bp sfr)))
  
;;;fu-recordとは、oid, sf-list を持つstructureである。
;;;           oid: op-id (number)
;;;           sf-list: list of sf-record
(defstruct fu-record
  oid
  sf-list ;list of sf-record
  )

;;fu-mixinは、data-unit classに、fu-record属性を加えるための、parent mix-inである。
(defclass fu-mixin ()
    ((sf-list :accessor sf-list
              :initform nil
              :initarg :sf-list)))

(defmethod max-fu-year ((p fu-mixin))
  (let ((sfl (sf-list p)))
    (if sfl
        (/ (apply #'max (mapcar #'sf-record-month (sf-list p)))
           12)
        nil)))

(defclass data-unit (operation fu-mixin)
  ((listhesis :accessor listhesis
              :initform nil
              :initarg :listhesis)))

(defmethod follow-up-years ((o operation) current-year)
  (- current-year (op-year o)))

;;sflistのなかに、month n が存在するかどうかを調べる関数
(defun find-n-in-sflist (n sflist)
  (find n sflist :test #'eql :key #'sf-record-month))

;;print-objectのための補助関数
;;sfrがnilであれば、NILを、そうでなければ、sf-recordの内容を印刷する
(defun print-sfr-or-nil (sfr stream)
  (if sfr
      (format stream "PS: ~A, MS: ~A, BP: ~A~%" (sf-record-ps sfr) (sf-record-ms sfr) (sf-record-bp sfr))
      (format stream "NIL")))

;;data-unitのための、print-objectのover-ride method
(defmethod print-object :after ((du data-unit) stream)
  (format stream "Listhesis: ~A~%" (listhesis du))
  (dolist (month '(0 6 12 24 36 48 60))
    (let ((sfl (sf-list du)))
      (format stream "Month ~A: " month)
      (let ((record-of-the-month (find-n-in-sflist month sfl)))
        (print-sfr-or-nil record-of-the-month stream)
        (format stream "~%")))))

(defmethod latex-output :after ((du data-unit) stream)
  (format stream "~%~%Listhesis: ~A~%~%" (listhesis du))
  (dolist (month '(0 6 12 24 36 48 60))
    (let ((sfl (sf-list du)))
      (format stream "\\vspace{0.5cm}~%")
      (format stream "Month ~A: " month)
      (let ((record-of-the-month (find-n-in-sflist month sfl)))
        (print-sfr-or-nil record-of-the-month stream)
        (format stream "~%~%")))))

(defvar *listhesis-cases* (intersection *oid-list* (mapcar #'op-id (op-list-from-disease-id 165))))
(setf *listhesis-cases* (union '(383 501 588 635 832 799 819) *listhesis-cases*)) ;listhesis caeの追加分

(defun spondylop (oid)
  (member oid *listhesis-cases* :test #'eql))

;;;spondylop: integer -> boolean
;;;This function takes an op_id and returns a boolean value of whether this op-id is related to
;;;the diagnosis of lumbar spondylolisthesis
;(defun spondylop (oid)
;	   (postmodern:query (:select 'op_id
;				      :from 'op_diag
;				      :where 
;				      (:and
;				       (:= 'op_id oid)
;				       (:= 'disease_id 165)))))


;;data-unit classの、generator関数
(defun create-data-unit (op fr)
  (let ((obj (make-instance 'data-unit
                            :op-id (op-id op)
                            :patient-id (patient-id op)
                            :kanji-name (kanji-name op)
                            :address (address op)
                            :phone (phone op)
                            :op-date (op-date op)
                            :start-time (start-time op)
                            :end-time (end-time op)
                            :preop-dx (preop-dx op)
                            :postop-dx (postop-dx op)
                            :procedure (procedure op)
                            :surgeons (surgeons op)
                            :assistants (assistants op)
                            :indication (indication op)
                            :op-note (op-note op)
                            :emergency (emergency op))))
    (if fr
        (setf (sf-list obj) (fu-record-sf-list fr)))
    (if (spondylop (op-id op))
        (setf (listhesis obj) t))
    obj))
  
;;;DEPRECIATED!!!
;;;CLASS op-with-fu-list
;;;  subclass of operation
;;;  Additional slot of the list of fu-records
(defclass op-with-fu-record (operation)
  ((fu-record :accessor fu-record
            :initarg fu-record
            :initform nil)))

;;;DEPRECIATED!!!
;;;op-with-fu-recordを、operationのデータから作成する関数。
;;;fu-record は、defaultで、nilになる。
(defmethod create-op-with-fu-record ((op operation))
  (make-instance 'op-with-fu-record
                 :op-id (op-id op)
                 :patient-id (patient-id op)
                 :kanji-name (kanji-name op)
                 :address (address op)
                 :phone (phone op)
                 :op-date (op-date op)
                 :start-time (start-time op)
                 :end-time (end-time op)
                 :preop-dx (preop-dx op)
                 :postop-dx (postop-dx op)
                 :procedure (procedure op)
                 :surgeons (surgeons op)
                 :assistants (assistants op)
                 :indication (indication op)
                 :op-note (op-note op)
                 :emergency (emergency op)))

;;;DEPRECIATED!!!
;;;operationのデータと、fu-recordを組み合わせて、op-with-fu-recordを作成する関数
(defmethod create-op-with-fu-record-2 ((op operation) fu-record)
  (let ((p (create-op-with-fu-record op)))
    (setf (fu-record p) fu-record)
    p))

;;;DEPRECIATED!!!
;;;op-with-fu-recordの内容を、リストとして返す関数
(defun describe-op-with-fu-record (p)
		(list
		 (op-id p)
		 (op-date p)
		 (kanji-name p)
		 (procedure p)
		 (if (fu-record p)
		     (mapcar #'sf-record-month (fu-record-sf-list (fu-record p)))
		     nil)))

;;;*data-table*作成のための補助関数
;;create-data-table: list-of-operation, list-of-fu-record |-> list-of data-unit
;;;operationのlistと、fu-recordのlistを読み込んで、それぞれのデータから得られる、data-unitのリストを返す関数
(defun create-data-table (oplist fudata)
  (labels ((attach-op-with-fu-record (op fudata)
             (create-data-unit op (find-if #'(lambda (x) (eql (fu-record-oid x) (op-id op))) fudata))))
    (cond
      ((endp oplist) nil)
      (t
       (cons (attach-op-with-fu-record (first oplist) fudata) (create-data-table (rest oplist) fudata))))))

;;;DEPRECIATED!!! 上記、create-data-tableに変更された
;;;convert-oplist: list_of_operation, list_of_fu_record |-> list_of op-with-fu-record
;;;operationのlistと、fu-recordのlistを読み込んで、それぞれのデータから得られる、op-with-fu-recordのリストを返す関数
(defun convert-oplist (oplist fudata)
  (labels ((attach-op-with-fu-record (op fudata)
             (create-op-with-fu-record-2 op (find-if #'(lambda (x) (eql (fu-record-oid x) (op-id op))) fudata))))
    (cond
      ((endp oplist) nil)
      (t
       (cons (attach-op-with-fu-record (first oplist) fudata) (convert-oplist (rest oplist) fudata))))))

;;;*study-table*  databaseから得た情報をもとに構築された、op-with-fu-record のリスト
;;(defvar *study-table* (sort (convert-oplist *oplist* *fudata*)) #'(lambda (x y) (simple-date:time< (op-date x) (op-date y))))

;;;update-study-table
;;;*oplist* と *fudata* を前提として、study tableを、下記のファイルに書き出す関数である。
(defun update-study-table ()
  (with-open-file (file "/Users/chang/Documents/Lumbar_outcome/study_table"
                        :if-exists :overwrite :if-does-not-exist :create :direction :output)
    (my-util:lists-to-csv file
                          (mapcar #'describe-op-with-fu-record
                                  *study-table* ) "|")))


(defmethod member-fu (month fu)
  (member month (fu-record-sf-list fu) :key #'(lambda (x) (sf-record-month x))))

;;;add-sf-record: sf-record fu-record -> fu-record
;;;add-sf-recordは、一つのsf-recordと、一つのfu-recordを受け取り、
;;;そのfu-recordのsf-listに、sf-recordを追加した、新たなfu-recordを返す関数である。
;;;作成されたsf-listは、自動的に、monthでsortされたものになる。
;;;前提：引数のsf-recordと、fu-recordは、同じop-idを持っていること。
(defun add-sf-record (sf-record fu-record)
  (labels
      ((add-record (sfr lo-sf-record)
	 (cond
	   ((endp lo-sf-record) (list sfr))
	   (t (cond ((< (sf-record-month sfr)
			(sf-record-month (first lo-sf-record)))
		     (cons sfr lo-sf-record))
		    (t (cons (first lo-sf-record)
			     (add-record sfr
					 (rest lo-sf-record)))))))))
    (make-fu-record :oid (fu-record-oid fu-record)
		    :sf-list (add-record sf-record
					 (fu-record-sf-list fu-record)))))
  

;;;extract-lo-month: lo-fu-record -> lo-lo-month
;;;extract-lo-monthは、fu-recordのlistを受け取り、それぞれのfollow upでの、monthのlist
;;;を返す。
(defun extract-lo-month (lo-fu-record)
  (cond
    ((endp lo-fu-record) nil)
    (t (cons (sf-list-2-lo-month (fu-record-sf-list (first lo-fu-record)))
	     (extract-lo-month (rest lo-fu-record))))))

;;;sf-list-2-lo-month: lo-sf-record -> lo-number
;;;sf-list-2-lo-monthは、sf-recordのlistを受け取り、そのリストに含まれる、month
;;;のデータのリストを、ソートされた形で出力する。
(defun sf-list-2-lo-month (lo-sf-record)
  (let ((result-list
	 (cond
	   ((endp lo-sf-record) nil)
	   (t (cons (sf-record-month (first lo-sf-record))
		    (sf-list-2-lo-month (rest lo-sf-record)))))))
    (sort result-list #'<)))

;;;pid-2-oid: string -> number または nil
;;;pid-2-oidは、patient_idの文字列を受け取り、patient databaseと照合して、その患者の、
;;;最も古いop-idを返す関数である。
;;;　　　op-idが存在しない場合nilを返す。
;;;前提：require postmodern, toplevelにpatient databaseがconnectされていること。
(defun pid-2-oid (pid)
  (let ((op-list (mapcar #'first (postmodern:query (:select 'op_id :from 'op
					    :where
					    (:= 'patient_id pid))))))
    (cond ((endp op-list) nil)
	  (t (apply #'min op-list)))))

;;;extract-sf-record: none -> lo-sf-record
;;;extract-sf-recordは、patient databaseの、sf36 tableを参照し、データを抽出する。
;;;抽出したデータは、pid-2-oid関数を使って、patient_idを、その患者の、
;;;最も小さいop_idを持つ手術のop_idに変換し、sf-recordを作成して、
;;;最終的に、sf-recordのlistを出力する。
;;;前提：require postmodern, toplevelにpatient databaseとのconnectがあること
(defun extract-sf-record ()
  (let ((lo-raw-data (postmodern:query (:select 'patient-id 'timing 'pf 'rp 'bp 'gh 'vt 'sf 're 'mh
						:from 'sf36))))
    (labels ((raw-data-2-sf-record (raw-data)
	       (make-sf-record :op-id (pid-2-oid (first raw-data))
			       :month (second raw-data)
			       :ps (float (/ (apply #'+ (subseq raw-data 2 6)) 4))
			       :ms (float (/ (apply #'+ (subseq raw-data 6 10)) 4))
                               :bp (fifth raw-data))))
      (mapcar #'raw-data-2-sf-record lo-raw-data))))

;;;sf-record-2-fu-record: sf-record -> fu-record
;;;sf-record-2-fu-recordは、sf-recordを受け取り、その情報を使って、その一つだけのsf-list
;;;を持つ、fu-recordを作成する関数である。
(defun sf-record-2-fu-record (sfr)
  (make-fu-record :oid (sf-record-op-id sfr) :sf-list (list sfr)))

;;;insert-sf-record: sf-record lo-fu-record -> lo-fu-record
;;;insert-sf-recordは、一つのsf-recordと、lo-fu-recordを受け取り、
;;; そのrawdataを、適切な位置にinsertして、updateされた lo-fu-recordを返す関数である。
(defun insert-sf-record (sf-record lo-fu-record)
  (cond
    ((endp lo-fu-record) (list (sf-record-2-fu-record sf-record)))
    (t (cond
	 ((= (sf-record-op-id sf-record)
	     (fu-record-oid (first lo-fu-record)))
	  (cons (add-sf-record sf-record (first lo-fu-record))
		(rest lo-fu-record)))
	 (t (cons (first lo-fu-record) (insert-sf-record sf-record (rest lo-fu-record))))))))

;;;construct-fu-data: lo-sf-record -> lo-fu-record
;;;construct-fu-dataは、lo-rawdataを受け取り、それから、fu-recordのlistを
;;;作成する関数である。
;;;Example
;;;
;;;Template
;;;    (defun construct-fu-data (lo-rawdata)
;;;       (cond
;;;        ((endp lo-rawdata) nil)
;;;        (t  (first lo-rawdata)... (construct-fu-data (rest lo-rawdata))...
(defun construct-fu-data (lo-sf-record)
  (cond
    ((endp lo-sf-record) nil)
    (t (insert-sf-record (first lo-sf-record)
			 (construct-fu-data (rest lo-sf-record))))))

;;;extract-fu-records nil -> list of fu-record
;;;This function extracts and constructs the list of fu-record from sf36 table, and then
;;;filters it using the list of op of the studied series to obtain the list of fu-record necessary for the study
;;;Make *Filtered-list* using the following command
;;;(load-filtered-list "/Users/chang/Documents/Lumbar_outcome/lumbar_list")
(defun extract-fu-records ()
	   (remove-if-not #'(lambda (x)
			    (member (fu-record-oid x) (mapcar #'op-id *oplist*))) (construct-fu-data (extract-sf-record))))

;;;*fudata*　SF36のデータから、*fudata*を定義する。これは、study tableの作成に使用される。
(defparameter *fudata* (extract-fu-records))

(defparameter *data-table* (sort (create-data-table *oplist* *fudata*) #'(lambda (x y) (simple-date:time< (op-date x) (op-date y)))))

(defun latex-out-data-table (data-table stream)
  (format stream "\\documentclass[a4, 12pt]{jarticle} \\setlength{\\parindent}{0cm}\\begin{document}")
  (dolist (x data-table)
    (latex-output x stream)
    (format stream "\\newpage "))
  (format stream "\\end{document}"))



;;;prepare-follow-up-data: none -> lo-fu-record
;;;prepare-follow-up-dataは、patient databaseのsf36 tableを参照し、そのデータを集めて、
;;;データ解析の最も基本となる、fu-recordのlistとして出力する。
;;;     データ解析のはじめに行う作業である。
;;;前提：require postmodern, toplevelで、patient databseにconnectしていること
;(defun prepare-follow-up-data ()
;  (construct-fu-data (extract-sf-record)))

;;;Multiple-op: string -> booleanまたは"error"
;;;multiple-opは、patient_idを受け取り、その患者が複数の手術を受けている場合、tを
;;;一回のみの場合にnilを、それ以外の場合に"error"を返す。
;;;前提
;;;   toplevelに、patient databaseが接続されていること
;;;Example
;;;   (multiple-op "00151803") = t
;;;   (multiple-op "00158170") = nil
;;;   (multiple-op "00158171") = "error"
(defun multiple-op (pid)
  (let* ((ret-val (postmodern:query (:select 'op-id
					    :from 'op
					    :where
					    (:= 'patient_id pid))))
	 (num (length ret-val)))
    (cond
      ((> num 1) t)
      ((= num 1) nil)
      (t "error"))))

(defun get-op-list (pid)
  (postmodern:query (:order-by
		     (:select
		      'o.op_id 'o.op_date
		      'p.kanji_name 'o.preop_dx 'o.procedure
		      :from (:as 'op 'o)
		      :left-join (:as 'patient 'p)
		      :on (:= 'p.patient_id 'o.patient_id)
		      :where (:= 'p.patient_id pid))
		     'o.op_date)))

;;;extract-op-for-further-question: lo-fu-record -> lo-fu-record
;;;この関数は、fu-recordのlistを受け取り、follow-up期間の超過したrecordのみを、抽出して
;;;出力する、filter関数である。
;;;
;;;Template
;;; (defun extract-op-for-further-question (fu-list)
;;;   (cond
;;;     ((endp fu-list) nil)
;;;     (t (cond
;;;          (fu-passed (first fu-list)) (cons (first fu-list)
;;;                                            (extract-op-for-further-question (rest fu-list))))
;;;          (t (extract-op-for-further-question (rest fu-list))))))
(defun extract-op-for-further-question (lo-fu-record)
  (remove-if-not #'fu-passed-p lo-fu-record))

;;;fu-passed-p: fu-record -> boolean
;;;この関数は、fu-recordを受け取り、そのop-dateと、本日の日付を比較して、passed-timeを計算し、
;;;それをもとに、fu-recordを調べ、questionnaier送付が必要かどうかを判断する関数である。
;;;必要と判断する論理は、
;;;    passed-time < 6   nil
;;;    passed-time < 12  6 があれば、nil なければ　t
;;;    passed-time < 24  12があれば、nil なければ　t
;;;    passed-time < 36  24があれば、nil なければ　t
;;;    passed-time < 48  36があれば、nil なければ　t
;;;
;;;    op-dateがnilの時は、nilを返す。
(defun fu-passed-p (fr)
  (let* ((op-date (fu-record-2-op-date fr))
	 (today (simple-date:universal-time-to-timestamp (get-universal-time)))
	 (passed-time (time-subtract-months today op-date)))
    (if passed-time
	(cond
	  ((> passed-time 60) (if (fr-contains fr 60) nil t))
	  ((> passed-time 48) (if (fr-contains fr 48) nil t))
	  ((> passed-time 36) (if (fr-contains fr 36) nil t))
	  ((> passed-time 24) (if (fr-contains fr 24) nil t))
	  ((> passed-time 12) (if (fr-contains fr 12) nil t))
	  ((> passed-time 6) (if (fr-contains fr 6) nil t))
	  ((> passed-time 0) (if (fr-contains fr 0) nil t)))
	nil)))
    
;;;fu-record-2-op-date: fu-record -> simple-date:time
;;;この関数は、follow-up-recordを受け取り、patient databseにアクセスして、その、op-date
;;;を、simple-dateでencodeしたものを返す。
(defun fu-record-2-op-date (fr)
 (simple-date:universal-time-to-timestamp
  (first (first (postmodern:query (:select 'op_date :from 'op
			     :where (:= 'op_id (fu-record-oid fr))))))))

;;;time-subtract-months: simple-date:time simple-date:time -> interval(months)
;;;この関数は、simple-date classでencodeされたtime ２個をとり、そのインターバルを
;;;何ヶ月かの整数で返す。
(defun time-subtract-months (time-1 time-2)
  (first (multiple-value-list
	  (floor (third (multiple-value-list
			 (simple-date:decode-interval
			  (simple-date:time-subtract time-1 time-2))))
		 30))))

;;;fr-contains: fu-record, integer -> boolean
;;;この関数は、fu-recordとintegerをとり、fu-recordに、integerのmonthのsf-recordが含まれていれば
;;;tを、そうでなければ、nilを返す
(defun fr-contains (fr i)
  (member i (fu-record-sf-list fr) :test 'equal :key #'(lambda (x) (sf-record-month x))))

;;;Essential functions

;;;oid-t-pid: integer -> string
;;;この関数は、op_idの整数値を取り、その手術のpatient_idのstringを返す。
(defun oid-2-pid (oid)
	   (first (first (postmodern:query (:select 'patient.patient_id :from 'patient
				    :left-join 'op :on (:= 'op.patient_id 'patient.patient_id)
				    :where (:= 'op.op_id oid))))))

(defun oid-2-kanjiname (oid)
  (first (first (postmodern:query (:select 'patient.kanji_name :from 'patient
					   :left-join 'op
					   :on (:= 'op.patient_id 'patient.patient_id)
					   :where (:= 'op.op_id oid))))))

;;;read-sf-data string -> list
;;;入力ファイルの名前をとり、処理した結果のリストを戻す。
;;;入力ファイルの書式は、comma-separated で、一番に、患者ID、その後に、sf36の８個の数値が並んでいる。
;;;ファイルを読み込み、それぞれの行を、リストにして、さらに、そのリストのリストを出力する。
(defun read-sf-data (file)
	   (let ((result-lst nil))
	     (with-open-file (stream file)
	       (do ((line (read-line stream nil 'eof)
			  (read-line stream nil 'eof)))
		   ((eql line 'eof) (nreverse result-lst))
		 (let ((templist (cl-ppcre:split ",\\s*" line)))
		   (setf result-lst (cons
				     (cons
				      (prep-id (car templist))
				      (convert-char-to-figure (rest templist)))
				     result-lst)))))))
;;;convert-char-to-figure: list -> list
;;;数値ストリングのリストを入力し、それらを数値に変換したリストを戻す
(defun convert-char-to-figure (inlist)
	   (cond
	     ((endp inlist) nil)
	     (t (cons (read-from-string (car inlist)) (convert-char-to-figure (rest inlist))))))

;;;insert-record: sf-record -> nil
;;;patient_id, timing, pf, rp, bp, gh, vt, sf, re, mh の並びのレコードを入力し、それを、toplevelでconnect
;;;されているdatabaseに、insertする。
(defun insert-record (record)
	   (multiple-value-bind (pid timing pf rp bp gh vt sf re mh) (values-list record)
	     (postmodern:query (:insert-into 'sf36
					     :set
					     'patient_id pid
					     'timing timing
					     'pf pf
					     'rp rp
					     'bp bp
					     'gh gh
					     'vt vt
					     'sf sf
					     're re
					     'mh mh))))
;;;dup-sf-recordp: (string, integer) -> boolean
;;;This function takes a list of (patient_id timing) and returns a boolean value
;;;true if there is already a record in the sf36 table with the same patient_id and timing
;;;nil if there is not.
(defun dup-sf-recordp (pid timing)
    (postmodern:query (:select 'patient_id
                               :from 'sf36
                               :where
                               (:and
                                (:= 'patient_id pid)
                                (:= 'timing timing)))))

;;;Depreciated!!
;;;collect-timing: (integer list-of-fu-record) -> list-of-sf-record
;;;This function takes the timing of interest and the list of fu-record, extracts the su-record with the
;;;pertinent timing, and returns the list of sf-record
;(defun collect-timing (timing lo-fu-record)
;	   (labels ((get-timing (timing fu-record)
;		      (find-if #'(lambda (x) (= timing (sf-record-month x))) (fu-record-sf-list fu-record))))
;	     (cond
;	       ((endp lo-fu-record) nil)
;	       (t
;		(if (get-timing timing (first lo-fu-record))
;				(cons
;				 (get-timing timing (first lo-fu-record))
;				 (collect-timing timing (rest lo-fu-record)))
;				(collect-timing timing (rest lo-fu-record)))))))

;;;collect-timing: (integer list-of-data-unit) -> list-of-sf-record
;;;This function takes the timing of interest and the list of data-unit, extracts the sf-record with the
;;;pertinent timing, and returns the list of sf-record
(defun collect-timing (timing lo-data-unit)
	   (labels ((get-timing (timing du)
		      (find-if #'(lambda (x) (= timing (sf-record-month x))) (sf-list du))))
	     (cond
	       ((endp lo-data-unit) nil)
	       (t
		(if (get-timing timing (first lo-data-unit))
				(cons
				 (get-timing timing (first lo-data-unit))
				 (collect-timing timing (rest lo-data-unit)))
				(collect-timing timing (rest lo-data-unit)))))))


;;;filter-year (integer list-of-operation) -> list-of-operation
;;;この関数は、operation classのリストから、手術日がある年のものを抜き出す
(defun filter-year (year op-list)
  (remove-if-not #'(lambda (x) (equal year (my-util:date-element 'year (op-date x))))
			op-list))

;;;calculate-ps: integer, function -> float, float, int
;;;This function takes the value of timing and a function to extract a list of fu-record
;;;and returns the calculated statistical data of physical score
(defun calculate-ps (timing ext-function)
	   (statistics:mean-sd-n (mapcar #'sf-record-ps (collect-timing timing (Funcall ext-function)))))

(defun calculate-bp (timing ext-function)
	   (statistics:mean-sd-n (mapcar #'sf-record-bp (collect-timing timing (Funcall ext-function)))))


(defmethod age-from-op ((op operation))
  "Return the age of the patient drawn from the indication of the op"
  (let ((indication (indication op)))
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
  (statistics:mean-sd-n (mapcar 'age-from-op op-lst)))

(defun oid-2-sex (oid)
  (caar
   (postmodern:query (:select 'sex
                              :from 'patient
                              :left-join 'op
                              :on (:= 'op.patient_id 'patient.patient_id)
                              :where (:= 'op.op_id oid)))))

;;;Raw dataのグラフを作るための関数
(defun element-for-graph (sflst month item)
		(let ((record (find-if
			       #'(lambda (x) (= (sf-record-month x) month))
			       sflst)))
		  (if record
		      (extract-from-sf-record item record)
		      "NA")))

(defun extract-from-sf-record (item record)
  (cond
    ((eq item 'ps) (sf-record-ps record))
    ((eq item 'ms) (sf-record-ms record))
    ((eq item 'bp) (sf-record-bp record))))

(defun output-data-to-file (filename)
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format s "op_id ps-0 ps-6 ps-12 ps-24 ps-36 ps-48 ps-60 bp-0 bp-6 bp-12 bp-24 bp-36 bp-48 bp-60 listhesis~%")
(dolist (d-record *data-table*)
  (let ((sflst (sf-list d-record)))
    (format s "~A ~f ~f ~f ~f ~f ~f ~f ~f ~f ~f ~f ~f ~f ~f ~d~%"
	    (op-id d-record)
	    (element-for-graph sflst 0 'ps)
	    (element-for-graph sflst 6 'ps)
	    (element-for-graph sflst 12 'ps)
	    (element-for-graph sflst 24 'ps)
	    (element-for-graph sflst 36 'ps)
	    (element-for-graph sflst 48 'ps)
	    (element-for-graph sflst 60'ps)
            (element-for-graph sflst 0 'bp)
            (element-for-graph sflst 6 'bp)
            (element-for-graph sflst 12 'bp)
            (element-for-graph sflst 24 'bp)
            (element-for-graph sflst 36 'bp)
            (element-for-graph sflst 48 'bp)
            (element-for-graph sflst 60 'bp)
            (if (member (op-id d-record) *listhesis-cases*)
                1
                0))))))

;;;(integer, data-unit) :=> float
;;;This function takes (month, data-unit) and returns the ps value of the month of this case
(defun get-ps-month-from-du (month du)
  (let ((pertinent-record (find-n-in-sflist month (sf-list du))))
    (if pertinent-record
        (sf-record-ps pertinent-record)
        nil)))

;;;(integer, data-unit) :=> float
;;;This function takes (month, data-unit) and returns the bp value of the month of this case
(defun get-bp-month-from-du (month du)
  (let ((pertinent-record (find-n-in-sflist month (sf-list du))))
    (if pertinent-record
        (sf-record-bp pertinent-record)
        nil)))

;;;(integer, data-unit) :=> float
;;;This function takes (month, data-unit) and returns the ms value of the month of this case
(defun get-ms-month-from-du (month du)
  (let ((pertinent-record (find-n-in-sflist month (sf-list du))))
    (if pertinent-record
        (sf-record-ms pertinent-record)
        nil)))

;;;data-unit, stream :=> nil
;;;This function takes a data-unit, and a stream, and sends the formatted output of the data-unit into the stream
(defun output-ulbd-data (du stream)
		(format stream (concatenate 'string
				       "* ~A~%:PROPERTIES:~%:patient_id: ~A~%:op_date: ~A~%"
                                       ":listhesis: ~A~%"
				       ":ps-0: ~A~%:ps-6: ~A~%:ps-12: ~A~%:ps-24: ~A~%"
				       ":ps-36: ~A~%:ps-48: ~A~%:ps-60: ~A~%"
                                       ":bp-0: ~A~%:bp-6: ~A~%:bp-12: ~A~%:bp-24: ~A~%:bp-36: ~A~%:bp-48: ~A~%:bp-60: ~A~%"
                                       ":END:~%")
		      (kanji-name du) (patient-id du)
                      (my-util::my-decode-date (op-date du))
                      (listhesis du)
		      (get-ps-month-from-du 0 du)
		      (get-ps-month-from-du 6 du)
		      (get-ps-month-from-du 12 du)
		      (get-ps-month-from-du 24 du)
		      (get-ps-month-from-du 36 du)
		      (get-ps-month-from-du 48 du)
		      (get-ps-month-from-du 60 du)
                      (get-bp-month-from-du 0 du)
                      (get-bp-month-from-du 6 du)
                      (get-bp-month-from-du 12 du)
                      (get-bp-month-from-du 24 du)
                      (get-bp-month-from-du 36 du)
                      (get-bp-month-from-du 48 du)
                      (get-bp-month-from-du 60 du)))

;;;data-unit, stream :=> nil
;;;This function takes a data-unit and a stream, and then outputs the data in the format for
;;;laminoplasty patient series into the stream
(defun output-laminoplasty-data (du stream)
  (format stream (concatenate 'string
                              "* ~A~%:PROPERTIES:~%:patient_id: ~A~%:op_date: ~A~%:level: ~%"
                              ":ps-0: ~A~%:ps-6: ~A~%:ps-12: ~A~%:ps-24: ~A~%"
                              ":ps-36: ~A~%:ps-48: ~A~%:ps-60: ~A~%:ps-0: ~%:vas-0: ~%:END:~%")
          (kanji-name du) (patient-id du)
          (my-util::my-decode-date (op-date du))
          (Listhesis du)
          (get-ps-month-from-du 0 du)
          (get-ps-month-from-du 6 du)
          (get-ps-month-from-du 12 du)
          (get-ps-month-from-du 24 du)
          (get-ps-month-from-du 36 du)
          (get-ps-month-from-du 48 du)
          (get-ps-month-from-du 60 du)))

(defun output-basic-data (du stream)
  (format stream (concatenate 'string
                              "* ~A~%:PROPERTIES:~%:patient_id: ~A~%:op_id: ~A~%:op_date: ~A~%:preop_dx: ~A~%:procedure: ~A~%:level: ~%"
                              ":ps-0: ~%:ps-6: ~%:ps-12: ~%:ps-24: ~%"
                              ":ps-36: ~%:ps-48: ~%:ps-60: ~%:bp-0: ~%:vas-0: ~%:END:~%")
          (kanji-name du) (patient-id du)
          (op-id du) (my-util::my-decode-date (op-date du)) (preop-dx du) (procedure du)))

(defun output-lumbar-data (du stream)
  (format stream (concatenate 'string
                              "* ~A~%:PROPERTIES:~%:patient_id: ~A~%:op_id: ~A~%:op_date: ~A~%:preop_dx: ~A~%:procedure: ~A~%"
                              ":listhesis: ~%:level: ~%"
                              ":ps-0: ~%:ps-6: ~%:ps-12: ~%:ps-24: ~%"
                              ":ps-36: ~%:ps-48: ~%:ps-60: ~%:bp-0: ~%:vas-0: ~%:END:~%")
          (kanji-name du) (patient-id du)
          (op-id du) (my-util::my-decode-date (op-date du)) (preop-dx du) (procedure du)))
           

;;;This function output the follow-up data of the 104 cases into a org-format file
;;;The target file is "/Users/chang/Desktop/lumbar_series.org"
(defun output-ulbd-file ()
    (with-open-file (str "/Users/chang/case_series/ULBD_series.org" :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format str (concatenate 'string
                               "#+COLUMNS: %17ITEM %10patient_id %12op_date %5listhesis %10ps-0 %10ps-6 %10ps-12 %10ps-24 %10ps-36 %10ps-48 %10ps-60"
                               " %10bp-0 %10bp-6 %10bp-12 %10bp24 %10bp36 %10bp48 %10bp60"))
      (mapcar #'(lambda (x) (output-ulbd-data x str)) *data-table*)))

;;;This function appends the data of op-id into the laminoplasty case_series
;;;sf-list is set to nil.
;;;File is specified as "/Users/chang/case-series/laminoplasty_series.org"
;;;Error is returned if the specified file does not exist
(defun append-to-laminoplasty (op-id)
  (with-open-file (str "/Users/chang/case_series/laminoplasty_series.org" :direction :output :if-exists :append)
    (output-laminoplasty-data (create-data-unit (make-instance-of-op-from-id op-id) nil) str)))

(defun append-to-foraminotomy (op-id)
  (with-open-file (str "/Users/chang/case_series/foraminotomy_patients_new.org" :direction :output :if-exists :append)
    (output-basic-data (create-data-unit (make-instance-of-op-from-id op-id) nil) str)))

(defun append-to-anterior-fusion (op-id)
  (with-open-file (str "/Users/chang/case_series/anterior_fusion_series.org" :direction :output :if-exists :append
                       :if-does-not-exist :create)
    (output-basic-data (create-data-unit (make-instance-of-op-from-id op-id) nil) str)))

(defun append-to-ulbd (op-id)
  (with-open-file (str "/Users/chang/case_series/ULBD_series_added.org" :direction :output :if-exists :append
                       :if-does-not-exist :create)
    (output-lumbar-data (create-data-unit (make-instance-of-op-from-id op-id) nil) str)))

  

