;;; jea-string-util-test.el --- code to break long strings into 280 character chunks

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

;;; code to make working with string easier here are the unit tests

;;; Code:

(require 'jea-string-util)
(require 'jea-code-gen-test)

(defun jea-string-util--test()
	"Test all react functions."
	(let ((t1 (jea-test-text '(lambda ()
															(jea-string-ltrim "  	   	The quick fox jumped quickly."))
													 "The quick fox jumped quickly."))
				(t2 (jea-test-text '(lambda ()
															(jea-string-rtrim "The quick fox jumped quickly.  	  	"))
													 "The quick fox jumped quickly."))
				(t3 (jea-test-text '(lambda ()
															(jea-string-trim "	  	  	The quick fox jumped quickly.  	  	"))
													 "The quick fox jumped quickly."))
				(t4 (jea-test-text '(lambda ()
															(jea-string-util-camel-case-to-snake "HelloBraveNewWorld"))
													 "hello_brave_new_world"))
				(t5 (jea-test-text '(lambda ()
															(jea-string-upcase-snake-case-to-camel "james_anderson__was_here"))
													 "JamesAndersonWasHere"))
				(t6 (jea-test-number '(lambda ()
																(jea-string-get-print-format 13))
														 13))
				(t7 (jea-test-number '(lambda ()
																(jea-string-get-print-format 12.34))
														 12.34))
				(t8 (jea-test-number '(lambda ()
																(jea-string-get-print-format "14"))
														 14))
				(t9 (jea-test-number '(lambda ()
																(jea-string-get-print-format "543.21"))
														 543.21))
				(t10 (jea-test-text '(lambda ()
																(jea-string-get-print-format "hello"))
														"hello"))
				(t11 (jea-test-text '(lambda ()
																(jea-capitalize-first "currentSquares"))
														"CurrentSquares"))
				(t12 (jea-test-list '(lambda ()
															 (jea-find-string-all-indexes
																"The quick fox jumped quickly." "\\(qu\\)\\(ick\\)"))
														'((4 6) (6 9) (21 23) (23 26))))
				(t13 (jea-test-list '(lambda ()
															 (jea-find-string-all
																"The quick fox jumped quickly." "\\(qu\\)\\(ick\\)"))
														'("qu" "ick" "qu" "ick")))
				(t14 (jea-test-list '(lambda ()
															 (jea-string-upcase-p "UPPERCASE"))
														t))
				(t15 (jea-test-list '(lambda ()
															 (jea-string-downcase-p "lowercase"))
														t))
				(t16 (jea-test-text '(lambda ()
															 (jea-char-to-nato-phonetic-alphabet "a"))
														"Alpha"))
				(t17 (jea-test-text '(lambda ()
															 (jea-char-to-nato-phonetic-alphabet "Z"))
														"Zulu"))
				(t18 (jea-test-text '(lambda ()
															 (jea-char-to-morse-code "b"))
														"_..."))
				(t19 (jea-test-text '(lambda ()
															 (jea-char-to-morse-code "X"))
														"_.._"))
				(t20 (jea-test-text '(lambda ()
															 (jea-string-to-nato-phonetic-alphabet "test"))
														"Tango Echo Sierra Tango"))
				(t21 (jea-test-text '(lambda ()
															 (jea-string-to-morse-code "test"))
															 "_ . ... _")))
		(and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21)))

(jea-string-util--test)

(provide 'jea-string-util-test)

;;; jea-string-util-test.el ends here
