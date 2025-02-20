;;; jea-password-generate.el --- code to generate passwords

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

;;; code to generate passwords.  makes it easy to use org mode as a password manager

;;; Code:

(defun jea-make-pwrd()
	"Senerate a random string with chaaracters, numbers and punctuation."
  (interactive)
  (random t) ;; seed random
  (let ((mycharset "abcdefghijklmnopqrstyvwxyzABCDEFGHIJKLMNOPQRSTYVWXYZ")
		(mynumset "1234567890")
		(mypunctset "!@#$")
		(charlength 10)
		(numlength 4)
		(punctlength 3))
	(insert "\n")
	(dotimes (i charlength)
	  (insert (elt mycharset (random (length mycharset)))))
	(dotimes (i numlength)
	  (insert (elt mynumset (random (length mynumset)))))
	(dotimes (i punctlength)
	  (insert (elt mypunctset (random (length mypunctset)))))
	))

(provide 'jea-make-pwrd)

;;; jea-password-generate.el ends here
