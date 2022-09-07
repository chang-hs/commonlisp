(defun list-first-item (list)
	   (mapcar 'car list))

(defun get-surgeon-name (id)
  (if id
      (strip-white-space (caar (postmodern:query (:select 'surgeon_name
				    :from 'surgeons
				    :where (:= 'surgeon_id id)))))
      ""))

(defun make-surgeon-string (op_id)
	   (let ((string))
	     (dolist (surgeon-name (reverse (mapcar 'get-surgeon-name
		      (list-first-item (postmodern:query
					(:select 'surgeon_id
					 :from 'op_surgeon
					 :where (:= 'op_id op_id)))))) string)
	       (if string
		   (setq string (concatenate 'string surgeon-name "、 " string))
		   (setq string surgeon-name)))))

(defun make-assistant-string (op_id)
	   (let ((string ""))
	     (dolist (surgeon-name (reverse (mapcar 'get-surgeon-name
		      (list-first-item (postmodern:query
					(:select 'surgeon_id
					 :from 'op_assistant
					 :where (:= 'op_id op_id)))))) string)
	       (if string
		   (setq string (concatenate 'string surgeon-name "、 " string))
		   (setq string surgeon-name)))))

;;;Function to set the "surgeons" field of op table from the data obtained
;;;from op_surgeon table.
(defun set-surgeons (op-id)
  (let ((surgeon-string (make-surgeon-string op-id)))
    (if surgeon-string
        (postmodern::query (:update 'op :set 'surgeons (make-surgeon-string op-id)
                                    :where (:= 'op_id op-id))))))

;;;Function to set the "assistants field of op table from the data obtained
;;;from op-assistant table
(defun set-assistants (op-id)
  (let ((assistant-string (make-assistant-string op-id)))
    (if assistant-string
        (postmodern::query (:update 'op :set 'assistants assistant-string
				     :where (:= 'op_id op-id))))))

;;;Function to show the field "surgeons" in op table given the op_id
(defun find-surgeon (id)
	   (postmodern:query (:select 'surgeons :from 'op :where (:= 'op_id id))))

;;;Function to show the field "assistants" in op table given the op_id
(defun find-assistants (id)
	   (postmodern:query (:select 'assistants :from 'op :where (:= 'op_id id))))

;;;Function to list the op_id whose "surgeons" field is null
(defun list-no-surgeons ()
	   (list-first-item (postmodern::query (:order-by (:select 'op_id :from 'op
					    :where (:is-null 'surgeons)) 'op_id))))
(defun list-no-assistants ()
	   (list-first-item (postmodern::query (:order-by (:select 'op_id :from 'op
					    :where (:is-null 'assistants)) 'op_id))))

;;;Function to list the op_id whose "start_time" field is null
(defun list-no-start-time ()
	   (list-first-item (postmodern:query (:order-by (:select 'op_id :from 'op
				    :where (:is-null 'start_time)) 'op_id))))