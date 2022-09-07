;;;このfileに含まれているのは、/home/chang/bibliography/lumbar.bibのデータを、patieit databaseに取り込むための
;;;関数である。


;;requires the regex package
(require 'cl-ppcre)
(require 'postmodern)

(postmodern:connect-toplevel "literature" "chang" "" "localhost")

(defparameter *project* 4)

;;;line-to-authors
;;;   take an argument string "line", performs a regex search with a key of "author = ..."
;;;   returns a list of authors
(defun line-to-authors (line)
  (cl-ppcre:split "\\s*and\\s*"
                  (multiple-value-bind (start end cap-begins cap-ends)
                      (cl-ppcre:scan "^\\s*author\\s*=\\s*{(.*)}" line)
                    (if (null start)
                        nil
                        (subseq line (aref cap-begins 0) (aref cap-ends 0))))))

(defun line-to-title (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*title\\s*=\\s*{(.*)}" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

(defun line-to-journal (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*journal\\s*=\\s*{(.*)}" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

(defun line-to-year (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*year\\s*=\\s*(\\d{4})" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

(defun line-to-volume (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*volume\\s*=\\s*(\\d*)" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

(defun line-to-page (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*pages\\s*=\\s*{(.*)}" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

(defun line-to-annote (line)
  (multiple-value-bind (start end cap-begins cap-ends) (cl-ppcre:scan "^\\s*annote\\s*=\\s*{(.*)}" line)
    (if (null start)
        nil
        (subseq line (aref cap-begins 0) (aref cap-ends 0)))))

;;;insert-data
;;;   inserts bibliographical data except for authors and cards into the literature database
(defun insert-data (title journal year volume pages project)
  (let (source_id)
    (postmodern:query
     (:insert-into 'literature
                   :set 'title title 'journal journal 'year year 'volume volume 'pages pages 'project project))
    (setf source_id (postmodern:query
           (:select 'source_id :from 'literature
                    :where (:and (:= 'journal journal) (:= 'volume volume) (:= 'pages pages)))))
    (if (> (length source_id) 1)
        (car (car (reverse source_id)))
        (car (car source_id)))))
;;;insert-authors
;;;   inserts the author data into the authors table
(defun insert-authors (source-id authors)
  (dolist (author authors)
    (postmodern:query
     (:insert-into 'authors
                   :set 'source_id source-id 'author author))))
;;;insert-annote
;;;   inserts the annote data into the cards database
(defun insert-annote (source-id annote)
  (postmodern:query
   (:insert-into 'cards
                 :set 'source_id source-id 'content annote)))
  
;;;input-lumbar-bib
;;;   reads the file /home/chang/bibliography/lumbar.bib line by line and
;;;   inserts the bibliographical data into the database set on the toplevel
(defun input-lumbar-bib ()
  (let ((authors) (title) (journal) (year) (volume) (pages) (annote) (source-id))
    (with-open-file (stream "/home/chang/bibliography/lumbar.bib")
      (do ((line (read-line stream nil) (read-line stream nil)))
          ((null line))
        (cond
          ((cl-ppcre:scan "^}" line)
           (setf source-id (insert-data title journal year volume pages *project*))
           (insert-authors source-id authors)
           (insert-annote source-id annote))
          ((line-to-authors line) (setf authors (line-to-authors line)))
          ((line-to-title line) (setf title (line-to-title line)))
          ((line-to-journal line) (setf journal (line-to-journal line)))
          ((line-to-year line) (setf year (line-to-year line)))
          ((line-to-volume line) (setf volume (line-to-volume line)))
          ((line-to-page line) (setf pages (line-to-page line)))
          ((line-to-annote line) (setf annote (line-to-annote line))))))))

(defun get-author (id) (car (car (postmodern:query
	  (:select 'author :from 'authors
		   :where (:= 'source_id id))))))
(defun get-year (id)
	   (car (car (postmodern:query
	  (:select 'literature.year :from 'literature
		   :where (:= 'source_id id))))))

(defun get-card-data (num) (postmodern:query
	  (:select 'c.card_id 'c.source_id 'c.title 'c.content :from (:as 'cards 'c)
		   :where (:in 'source_id
			       (:select 'source_id :from 'literature
					:where (:= 'project num))))))

(defparameter *data* (get-card-data 4))

(defun prep-line (data) (list
                         (second data)
                         (get-author (second data))
                         (get-year (second data))
                         (third data)
                         (fourth data )))

(defun list-project (stream num)
  (dolist ( line (mapcar #'prep-line (get-card-data num)))
    (format stream "~A ~A, ~A~%~A~%~A~%~%"
            (first line)
            (second line)
            (third line)
            (fourth line)
            (fifth line))))

