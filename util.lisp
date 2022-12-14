(require 'cl-ppcre)
(require 'postmodern)
(require 'simple-date)
(defpackage :my-util
  (:use :common-lisp :cl-ppcre)
  (:export :strip-white-space :lists-to-csv :min-max :my-decode-date :date-element
           :simple-date-2-utime :unique-list))
(in-package :my-util)

;;;strip-white-space: string -> string
;;;This function strips the white-space from the string and returns the result
(defun strip-white-space (string)
  (if (not (eql string :NULL))
      (multiple-value-bind (result list)
          (cl-ppcre:scan-to-strings "(\\s|ã)*([^\\sã]*.*[^\\s])(\\s|ã)*$" string)
        (if result
            (elt list 1)))
      nil))

;;;lists-to-csv list-of-list -> void
;;;This function takes a stream, list of list, and a delimiter
;;;and output it as multiple lines of delimiter-separated items
;;;to the stream
(defun lists-to-csv (stream lines delimiter)
  (labels ((output-csv (stream line delimiter)
                      (cond
                        ((= (length line) 1)
                         (format stream "~20A~%" (first line)))
                        (t
                         (format stream "~A~A" (first line) delimiter)
                         (output-csv stream (rest line) delimiter)))))
    (cond
      ((endp lines) nil)
      (t
       (output-csv stream (first lines) delimiter)
       (lists-to-csv stream (rest lines) delimiter)))))

;;;min-max list-of-numbers -> list-of-numbers
;;;This function returns the minimal and the maximal numebrs in the list
(defun min-max (lst)
  (labels ((rec (sublst acc)
                (cond
                  ((endp sublst) acc)
                  (t
                   (if (< (car sublst) (car acc))
                       (rec (rest sublst) (cons (car sublst) (rest acc)))
                       (if (< (second acc) (car sublst))
                           (rec (rest sublst) (list (car acc)
                                                    (car sublst)))
                           (rec (rest sublst) acc)))))))
    (rec lst (list (car lst) (car lst)))))

(defun my-decode-date (date)
  (if (not (equal date :null))
      (multiple-value-bind (year month day)
          (simple-date:decode-date date)
        (format nil "~A-~A-~A" month day year))
      nil))

  
;;;date-element (symbol simple-date) -> integer
;;;ãã®é¢æ°ã¯ã'date, 'month, 'year ãªã©ã®symbolã®å¥åã«å¯¾ããsimple-dateã®ããã®å¤ãè¿ã
(defun date-element (element sdate)
	   (multiple-value-bind (second minute hour date month year)
	       (decode-universal-time (simple-date-2-utime sdate))
             (case element
	       ('date date)
	       ('month month)
	       ('year year))))

;;;simple-date-2-utime: simple-date -> universal-time
;;;This function converts a simple-date to a universal-time
(defun simple-date-2-utime (date)
	   (multiple-value-bind (yy mm dd) (simple-date:decode-date date)
	   (encode-universal-time 0 0 0 dd mm yy)))

;;;unique-list: list -> list
;;;unique-listã¯ãlistãåãåããéè¤ããè¦ç´ ãåé¤ãã¦ãuniqueãªãã®ã®ã¿
;;;ãããªãlistãè¿ãé¢æ°ã§ããã
;;;example
;;;  (unique-list '(1 3 4 3 5 2 1)) -> (1 2 3 4 5)
;;;
(defun unique-list (lst)
  (cond
    ((endp lst) nil)
    (t
     (let ((unique-rest-list (unique-list (rest lst))))
       (cond ((member (first lst) unique-rest-list :test #'equal)
	      unique-rest-list)
	     (t (cons (first lst) unique-rest-list)))))))

;;;remove-if-member (list list) -> list
;;;residual-item ã¯ãsecond-listãããfirst-listã«å«ã¾ãã¦ããitemãåé¤ãããªã¹ããè¿ãé¢æ°ã§ããã
(defun residual-item (smaller-set larger-set)
  (cond
    ((endp larger-set) nil)
    (t
     (cond
       ((member (first larger-set) smaller-set)
        (residual-item smaller-set (rest larger-set)))
       (t
        (cons (first larger-set) (residual-item smaller-set
                                                (rest larger-set))))))))
;;;delete-until-x (number list-of-object function/1(extract a number from object)) -> list-of-number
;;;ããçªå·ãåºã¦ããã¾ã§listãåããæ¶å»ãã¦ããé¢æ°
;;;ä»ã®ã¨ããããªã¹ãã®åå®¹ã¯ãæ°ã ãã
(defun delete-until-x (x lst fnc)
  (cond ((null lst) nil)
        ((= x (apply fnc (list (car lst))))
         lst)
        (t
         (delete-until-x x (cdr lst) fnc))))