;;; jea-vigenere-cipher-test.el --- code to test jea-vigenere-cipher.el

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

;;; https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher

;;; Code:

(require 'jea-vigenere-cipher)
(require 'jea-code-gen-test)

(defun jea--vc-test1 ()
	"The first test of Vigenère ciper."
	(let ((t1 (= (jea--vc-get-char-code "r") 17))
				(t2 (string-equal (jea--vc-get-code-char 2) "C"))
				(t3 (jea-test-text '(lambda ()
															(jea-vigenere-encrypt "attackingtonight" "oculorhinolaryngology"))
													 "OVNLQBPVTHZNZEUZ"))
				(t4 (jea-test-text '(lambda ()
															(jea-vigenere-decrypt "ovnlqbpvthznzeuz" "oculorhinolaryngology"))
													 "ATTACKINGTONIGHT")))
		(and t1 t2 t3 t4)))

(jea--vc-test1)

(provide 'jea-vigenere-cipher-test)

;;; jea-vigenere-cipher-test.el ends here
