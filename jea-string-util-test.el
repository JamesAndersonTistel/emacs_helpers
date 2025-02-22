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

(defun jea-string-ltrim--test1-data-in()
	"Data for testing the remove of leading spaces."
	"  	   	The quick fox jumped quickly.")

(defun jea-string-ltrim--test1-data-out()
	"Desired result for testing the remove of leading spaces."
	"The quick fox jumped quickly.")

(defun jea-string-ltrim--test1()
	"The first test of striping the left side of a string."
	(equal (jea-string-ltrim (jea-string-ltrim--test1-data-in))
				 (jea-string-ltrim--test1-data-out)))

;; (jea-string-ltrim (jea-string-ltrim--test1-data-in))
;; (jea-string-ltrim--test1)

(provide 'jea-string-util-test)

;;; jea-string-util-test.el ends here
