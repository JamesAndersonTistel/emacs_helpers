;;; jea-tweet-assist.el --- code to break long strings into 280 character chunks

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

;;; code to break long strings into 280 character chunks

;;; Code:

(require 'jea-string-util)

(defun jea-tweet--max-length()
	"The max size per social media post.
In the future maybe make it configurable per social media platform.
TODO converrt to defvar"
	280)

(defun jea-tweet--remove-extra-whitespace(in-str)
	"Remove newlines and extra (two or more) spaces.
\IN-STR is the arg to be cleaned up.  We remove newlines first.
then the inner places where there are more than one space back to
back.  Then we finally trim the leading and trailing spaces."

	(jea-string-trim
	 (replace-regexp-in-string "[ \t]\\{2,\\}" " "
														 (replace-regexp-in-string "[\n]+" "" in-str))))

(defun jea-tweet--split-into-sentences(in-str)
	"IN-STR is the arg."
	(jea-find-string-all in-str "\\([^.]+\\.\\)"))

;; (jea-tweet--test2-in-data)
;; (jea-tweet--remove-extra-whitespace  (jea-tweet--test2-in-data))
;; (jea-tweet--split-into-sentences (jea-tweet--remove-extra-whitespace (jea-tweet--test2-in-data)))


(defun jea-tweet--process(in-str)
	"IN-STR is the raw full string that we might need to break up into sub tweets."
	(let* ((step1 (jea-tweet--split-into-sentences in-str))
				 (step2 (mapcar #'jea-tweet--remove-extra-whitespace step1)))
		step2))

;; (jea-tweet--process (jea-tweet--test2-in-data))

(defun jea-tweet-split-long(in-str)
	"Split long string into 280 character chunks.
IN-STR is the raw full string that we might need to break up into sub tweets.
Try to keep sentences intact."
  (interactive)
	(if (< (length in-str) (jea-tweet--max-length))
			in-str
		(message "not short enough")))

(provide 'jea-tweet-assist)

;;; jea-tweet-assist.el ends here
