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

(defun jea-tweet--one-of-length()
	"How much space do we need to say length(' XX/XX') is 6 to mark the continuation."
	6)

(defun jea-tweet--split(sentence)
	"Return parts if the SENTENCE is splittable.
Otherwise, just return the sentence as is.  Right now urls only thing unsplittable."
	(cond ((string-match "http" (downcase sentence))
				 (seq-filter (lambda (s) (> (length s) 0)) (jea-find-string-all sentence "\\(.*\\)\\(http[^ ]+\\)\\(.*\\)")))
				(t (list sentence)))) ;; so all processing is the same

;; (jea-tweet--split "foo https://www.instagram.com/stories/usnavy/3573171440662206849/ bar")
;; (jea-tweet--split "https://www.instagram.com/stories/usnavy/3573171440662206849/")
;; (jea-tweet--split "https://www.instagram.com/stories/usnavy/3573171440662206849/ bar")
;; (jea-tweet--split "foo bar")

;;; ----------------------------------------------------------------------
;;; 1) break up into sentences

(defun jea-tweet--split-into-sentences(in-str)
	"IN-STR is the arg.  This part is very naive.
It just breaks sentences by period.  It messes up on urls though.
more thought need to go in for this problem."
	(jea-find-string-all in-str "\\([^.]+\\.\\)"))

;; (setq t1 (jea-tweet--split-into-sentences (jea-tweet--test2-in-data)))

;;; ----------------------------------------------------------------------
;;; 2) remove exta spaces to save room

(defun jea-tweet--remove-extra-whitespace(in-str)
	"Remove newlines and extra (two or more) spaces.
\IN-STR is the arg to be cleaned up.  We remove newlines first.
then the inner places where there are more than one space back to
back.  Then we finally trim the leading and trailing spaces."

	(jea-string-trim
	 (replace-regexp-in-string "[ \t]\\{2,\\}" " "
														 (replace-regexp-in-string "[\n]+" "" in-str))))

;; (setq t2 (mapcar #'jea-tweet--remove-extra-whitespace t1))

;;; ----------------------------------------------------------------------
;;; 3) go through the sentences and merge into paragraphs

(defun jea-tweet--merge-sentences-into-paragraphs(sentences)
	"SENTENCES hold all the lines of the text seperated."
	(let* ((max-size (- (jea-tweet--max-length) (jea-tweet--one-of-length)))
				 (para "")
				 (result '()))
		(dolist (sentence sentences)
			(if (< (+ (length para) (length sentence)) max-size)
					(setq para (concat para sentence " "))
				(progn
					(setq result (cons para result))
					(setq para  ""))))
		(setq result (cons para result))
		(reverse result)))

;; (setq t3 (jea-tweet--merge-sentences-into-paragraphs t2))

;;; ----------------------------------------------------------------------
;;; 4) decorate with the 1/x for total count

(defun jea-tweet--decorate-paragraphs(paragraphs)
	"PARAGRAPHS hold all the sentences."
	(let ((numerator 1)
				(denominator (length paragraphs))
				results '())
		(dolist (p paragraphs)
			(setq results (cons (concat (format "%s%d/%d" p numerator denominator)) results))
			(setq numerator (1+ numerator)))
		(reverse results)))

;; (setq t4 (jea-tweet--decorate-paragraphs t3))

;;; ----------------------------------------------------------------------
;;; main

(defun jea-tweet--main(in-str)
	"IN-STR raw long string to be processed."
	(let* ((step1 (jea-tweet--split-into-sentences in-str))
				 (step2 (mapcar #'jea-tweet--remove-extra-whitespace step1))
				 (step3 (jea-tweet--merge-sentences-into-paragraphs step2))
				 (step4 (jea-tweet--decorate-paragraphs step3)))
		(mapconcat (lambda (x) (format "%s\n\n" x)) step4)))

;; (setq t5 (jea-tweet--main (jea-tweet--test2-in-data)))

;;; ----------------------------------------------------------------------
;;; public

(defun jea-tweet-split-long(in-str)
	"Split long string into 280 character chunks.
IN-STR is the raw full string that we might need to break up into sub tweets.
 Try to keep sentences intact.  Things like URLs should not be split up."
  (interactive)
	(jea-tweet--main in-str))

;; (jea-tweet-split-long (jea-tweet--test2-in-data))q

(defun jea-tweet-split-long-buffer(in-str)
	"Dump the processed IN-STR into a buffer to cut and paste."
	(interactive)
	(save-excursion
		(with-current-buffer (get-buffer-create "*social-media-post*")
 			(progn
 				(goto-char (point-max))
				(insert (jea-tweet-split-long in-str))))))

;; (jea-tweet-split-long-buffer (jea-tweet--test2-in-data))

(provide 'jea-tweet-assist)

;;; jea-tweet-assist.el ends here
