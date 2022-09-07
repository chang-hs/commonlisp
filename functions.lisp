(defun create-opid-procedure-pair (pt)
  "Create a pair: op-id and procedure from patient"
  (cons
   (get-property :op-id pt)
   (get-property :procedure pt)))

(defun print-cons-pair (c &optional (limiter "\\t") &key (str t))
  (format str "~A~A~A" (car c) limiter (cdr c)))