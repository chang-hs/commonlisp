(require 'postmodern)

(defun put-word (word meaning)
  (postmodern:query
   (:insert-into 'dict :set 'word word 'meaning meaning)))

(defun search-word (word)
  "search a word from the database 'dict'"
  (postmodern:query
   (:select 'meaning :from 'dict :where (:= 'word word))))