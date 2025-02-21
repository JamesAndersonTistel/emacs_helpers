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
;;; TODO break out unit tests into another file

;;; Code:

(defun jea-find-all-indexes(in-str regex)
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

;; (jea-find-all-indexes "The quick fox jumped quickly." "\\(qu\\)\\(ick\\)")
;;                        0123456789           21
;; ((4 6) (6 9) (21 23) (23 26))

;; (jea-find-all-indexes "The quick fox jumped quickly." "\\(qu\\)")
;;                        0123456789           21
;; ((4 6) (21 23))

;; (jea-find-all-indexes "The quick fox jumped quickly." "fox")
;; nil

(defun jea-find-all(in-str regex)
	"Return all the text of the matching groups.
\IN-STR input string
\REGEX the regular expressing with groups."
	(let ((indexes (jea-find-all-indexes in-str regex))
				(start nil)
				(end nil)
				(results '()))
		(while indexes
			(setq start (car (car indexes)))
			(setq end (car (cdr (car indexes))))
			(setq indexes (cdr indexes))
			(setq results (cons (substring in-str start end) results)))
		(reverse results)))

;; (jea-find-all "The quick fox jumped quickly." "\\(qu\\)\\(ick\\)")
;; ("qu" "ick" "qu" "ick")

;; (jea-find-all "The quick fox jumped quickly." "\\(qu\\)")
;; ("qu" "qu")

;; (jea-find-all "The quick fox jumped quickly.  The fat fox waddled a bit." "\\(f[a-zA-Z]+x\\)")
;; ("fox" "fox")

;; (jea-find-all "The quick fox jumped quickly." "fox")
;; nil

(provide 'jea-string-util)

;;; jea-string-util.el ends here
