(defun for-insert-property-from-patient (stream p)
  (format stream "* ~A~%:PROPERTIES:~%:patient_id: ~A~%:op_id: ~A~%:op_date: ~A~%:preop_dx: ~A~%:procedure: ~A~%:opnote: ~A~%:sf-0:~%:sf-6:~%:sf-12:~%:sf-24:~%:sf-36:~%:vas-0:~%:vas-6:~%:vas-12:~%:vas-24:~%:vas-36:~%:END:~%"
          (kanji-name p)
          (patient-id p)
          (op-id p)
          (my-util:my-decode-date (op-date p))
          (preop-dx p)
          (procedure p)
          (format nil "[[file:/Volumes/chang/opnote/opnote-~A.pdf][opnote]]" (op-id p))))