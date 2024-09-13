#!/usr/bin/env ciel

;;; download the input of the aoc

(in-package :ciel-user)

(defparameter *this-folder*
  (uiop:pathname-directory-pathname (first *script-args*)))

(defvar *aoc-session*  (if *this-folder*
                           (-> (file-finder:finder* :root (file-finder:file *this-folder*)
                                                    :predicates (file-finder:name= "aoc-session"))
                               (first)
                               (file-finder:path)
                               (str:from-file)
                               (str:lines)
                               (car))))

(defvar *this-year* (nth 5 (multiple-value-list (get-decoded-time))) "this year")

(defun download-input (year-num day-num &key (session *aoc-session*) input-file-path)
  "Get the {day-num} input. Maybe write to input-file-path.
Need the session in cookie for authorizing."
  (let ((out (make-string-output-stream))
		content)
	(sb-ext:run-program "curl"
						(list "-sL"
							  "-H"
							  (format nil "cookie: session=~a" session)
							  (format nil "https://adventofcode.com/~a/day/~a/input" year-num day-num)
							  )
						:search t
						:output out)
	
	(setf content (get-output-stream-string out))
	
	(if input-file-path
		(with-open-file (s input-file-path :direction :output :if-does-not-exist :create :if-exists :overwrite)
		  (format s content))
	    (format t content))))

(defun cli/options ()
  (list
   (clingon:make-option
    :integer
    :description "year number"
    :short-name #\y
    :long-name "year"
    :key :year)
        
   (clingon:make-option
    :integer
    :description "day number"
    :short-name #\d
    :long-name "day"
    :key :day)

   (clingon:make-option
    :string
    :description "output file"
    :short-name #\o
    :long-name "output"
    :key :output)
   
   (clingon:make-option
    :flag 
    :description "show help"
    :short-name #\h
    :key :help)))

(defun cli/handler (cmd)
  (let ((help (clingon:getopt cmd :help)))
    (when help
      (clingon:print-usage cmd t)
      (format t "session is ~a~%" *aoc-session*)
      (return-from cli/handler)))

  (let ((y (clingon:getopt cmd :year *this-year*))
        (d (clingon:getopt cmd :day))
        (o (clingon:getopt cmd :output)))

    (if (null d)
        (error "ERROR: has to have day number"))
    
    (download-input y d :input-file-path o)))

(defun cli/command ()
  (clingon:make-command
   :name "aoc input picket"
   :version "0.1.0"
   :options (cli/options) 
   :handler 'cli/handler))

#+ciel
(clingon:run (cli/command) *script-args*)
