(require 'postmodern)
(require 'cl-ppcre)
(import 'postmodern:connect-toplevel)

(connect-toplevel "patient" "chang" "nfkc505" "localhost")

(defun slip-patient-list ()
  (postmodern:query (:order-by (:select 'p.patient_id 'p.kanji_name 'o.op_date 'o.procedure
                           :from (:as 'patient 'p)
                           :left-join (:as 'op 'o) :on (:= 'p.patient_id 'o.patient_id)
                           :where (:and (:= 'p.cold 'true) (:= 'p.listhesis 'true)))
                             'o.op_date)))
  


