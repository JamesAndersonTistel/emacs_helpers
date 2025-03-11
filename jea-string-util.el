;;; jea-string-util.el --- code to break long strings into 280 character chunks

;; Copyright © 2025 James Anderson
;;
;; Author: James Anderson <james@tisteltech.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;; software and associated documentation files (the "Software"), to deal in the Software
;; without restriction, including without limitation the rights to use, copy, modify,
;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; code to make working with string easier

;;; Code:

;; ------------------------------ string helpers ---------------------

(defun jea-string-ltrim(in-str)
	"Take in IN-STR and return the string with the leading spaces removed."
	(replace-regexp-in-string "^[ \t]*" "" in-str))

(defun jea-string-rtrim(in-str)
	"Take in IN-STR and return the string with the trailing spaces removed."
	(replace-regexp-in-string "[ \t]*$" "" in-str))

(defun jea-string-trim(in-str)
	"Take in IN-STR and return the string with whitespace removed."
	(jea-string-ltrim (jea-string-rtrim in-str)))

(defun jea-string-get-print-format(in-str)
	"If IN-STR is a string number convert to string.  Other wise just return."
	(if (integerp in-str)
			in-str
		(let* ((start (string-match	"^[0-9]+$" in-str))
					 (end (match-end 0))
					 (result (if start (string-to-number (substring in-str start end))
										 in-str)))
			result)))

;; (jea-string-get-print-format 13)
;; (jea-string-get-print-format "14")
;; (jea-string-get-print-format "hello")

;; ------------------------------ regex helpers ----------------------

(defun jea-find-string-all-indexes(in-str regex)
	"Return all the indexes of the matching groups.
\IN-STR input string
\REGEX the regular expressing with groups."

	(let ((current-start 0)
				(results '()))
		(while (string-match regex in-str current-start)
			;; now lets look for the groups
			(let ((current-group 1)
						(matched nil)) ;; if the regex has no parentheses we could get stuck in an infine loop
				(while (match-beginning current-group)
					(setq results (cons (list (match-beginning current-group)
																		(match-end current-group))
															results))
					(setq current-start (match-end current-group))
					(setq matched t)
					(setq current-group (1+ current-group)))
				(if (not matched)
						(setq current-start (length in-str)))))
		(reverse results)))

(defun jea-find-string-all(in-str regex)
	"Return all the text of the matching groups.
\IN-STR input string
\REGEX the regular expressing with groups."
	(let ((indexes (jea-find-string-all-indexes in-str regex))
				(start nil)
				(end nil)
				(results '()))
		(while indexes
			(setq start (car (car indexes)))
			(setq end (car (cdr (car indexes))))
			(setq indexes (cdr indexes))
			(setq results (cons (substring in-str start end) results)))
		(reverse results)))

;; --------------------------- code formatting -------------------------

(defun jea-string-upcase-p (in-str)
	"If IN-STR is upper case return t."
	(equal in-str (upcase in-str)))

(defun jea-string-downcase-p (in-str)
	"If IN-STR is lower case return t."
	(equal in-str (downcase in-str)))

(defun jea-string-util-camel-case-to-snake(in-str)
	"Convert IN-STR from CamelCase to snake_case."
	(let ((len (1- (length in-str)))
				(pos 0)
				(case-fold-search nil) ;; case sensitive
				(result ""))
		(while (and (> len 0) (< pos len))
			(let ((curr (substring in-str pos (+ pos 1)))
 						(next (substring in-str (1+ pos) (+ pos 2))))
				(cond ((and (jea-string-downcase-p curr) (jea-string-upcase-p next))
							 (setq result (concat result (downcase curr) "_")))
							(t (setq result (concat result (downcase curr))))))
			(setq pos (1+ pos)))
		(setq result (concat result (downcase (substring in-str (1- (length in-str)) (length in-str))))) ;; don't forget last one
		result))

(defun jea-string-upcase-snake-case-to-camel(in-str)
	"Convert IN-STR from snake_case to CamelCase."
	(let ((result (upcase (substring in-str 0 1)))
				(pos 1)
				(underscore nil) ;; hit a underscore
				(len (1- (length in-str))))
		(while (and (> len 0) (< pos len))
			(let ((curr (substring in-str pos (+ pos 1))))
				(cond
				 (underscore
					(if (not (equal "_" curr)) ;; there can be more than one in a row
							(progn
								(setq result (concat result (upcase curr)))
								(setq underscore nil))))
				 (t
					(if (equal "_" curr)
							(setq underscore t)
						(setq result (concat result (downcase curr)))))))
			(setq pos (1+ pos)))
		(setq result (concat result (downcase (substring in-str (1- (length in-str)) (length in-str))))) ;; don't forget last one
		result))

(provide 'jea-string-util)

;;; jea-string-util.el ends here
