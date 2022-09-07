(defpackage :operation
  (:use :common-lisp :my-util :postmodern :statistics)
  (:nicknames :op)
  (:export :connect-patient
           :latex-output
           :operation :op-id :op-year :patient-id :kanji-name :kana-name :birthdate :sex :zip-code :address :phone
           :op-date :start-time :end-time :patho-div :location :make-instance-of-op-from-id
           :preop-dx :postop-dx :procedure :surgeons :assistants :indication :op-note :emergency
           :make-instance-of-op-from-id :get-disease-id :op-list-from-disease-id :prep-id
           :mean-follow-up))

(defpackage :lumbar-study
  (:use :common-lisp :postmodern :simple-date :operation)
  (:export :*oplist* :*oid-list*
           :extract-fu-records :fu-record-oid :fu-record-sf-list :sf-record-op-id :sf-record-month
           :load-filtered-list :oid-2-pid :oid-2-kanjiname
           :dup-sf-recordp :insert-record
           :member-fu :filter-year :calculate-ps :calculate-bp
           :study-table-operation :study-table-furecord
           :append-to-foraminotomy :append-to-ulbd :append-to-anterior-fusion
           :append-to-laminoplasty))