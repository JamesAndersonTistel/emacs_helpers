;;; jea-tweet-assist-test.el --- test the code to break long strings into 280 character chunks

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

;;; test code to break long strings into 280 character chunks
;;; this needs to be improved into proper unit tests (TODO)

;;; Code:

(require 'jea-tweet-assist)
(require 'jea-code-gen-test)

(defun jea--tweet-assist-test-set1()
	"We need to make sure when we split a long social media post that we try to preserve un-split-able things."
	(let ((t1 (jea-test-list '(lambda ()
															(jea-tweet--split "foo https://www.example.com/stories/usnavy/357/ bar"))
													 '("foo " "https://www.example.com/stories/usnavy/357/" " bar")))
				(t2 (jea-test-list '(lambda ()
															(jea-tweet--split "https://www.example.com/stories/usnavy/35731/"))
													 '("https://www.example.com/stories/usnavy/35731/")))
				(t3 (jea-test-list '(lambda ()
															(jea-tweet--split "https://www.example.com/stories/usnavy/35731714/ bar"))
													 '("https://www.example.com/stories/usnavy/35731714/" " bar")))
				(t4 (jea-test-list '(lambda ()
															(jea-tweet--split "foo bar"))
													 '("foo bar")))
				)
		(and t1 t2 t3 t4)))

(defun jea--tweet-assist-test-set2()
	"Text split into sentences."
	(let ((t1 (jea-test-lambda '(lambda ()
										(nth 1 (jea-tweet--split-into-sentences (jea-test-get-long-random-text1))))
								 '(lambda (data)
										(stringp data)))))
		(and t1)))

(defun jea--tweet-assist-test()
	"Run the test."
	(and (jea--tweet-assist-test-set1)
			 (jea--tweet-assist-test-set2)))

(jea--tweet-assist-test)

;; ugh, not splitting quote might be tricky, there could be several
;; (jea-find-string-all "This string contains \"quotes are tricky\" that are fun to with." "\"\\([a-z A-Z]+\\)\"")
;; (jea-find-string-all "This string contains \"quotes are tricky\" that are fun to \"with\"." "\"\\([a-z A-Z]+\\)\"")

;; (defun jea-test-run()
;;   "Hook up F5 to run."
;;   (interactive)
;;   (with-current-buffer (get-buffer-create "*jea-code-gen*")
;;   	(erase-buffer)
;;   	(jea-cg--py-insert-class "dog" '("ssleepd" "ibark" "bdig" "sswim"))))
;;
;; (global-set-key [(f5)] 'jea-test-run)



;;; jea-tweet-assist-test.el ends here
