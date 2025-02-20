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

(defun jea-make-pwrd--shuffle(in-str)
	"Shuffle whole sequence to make sure chars are not always first etc.
Assumes random has been seeded.

\IN-STR is the whole sequence."
	(let ((result (copy-sequence in-str))
				(len (length in-str)))
		(dotimes (i len)
			(setf (seq-elt result i) (elt in-str (random len))))
		result))

(defun jea-make-pwrd-generate()
	"Produce a random string that is appropriate for a password.

TODO: in the future maybe take into account a passed in arg for size"
		(let ((mycharset "abcdefghijklmnopqrstyvwxyzABCDEFGHIJKLMNOPQRSTYVWXYZ")
					(mynumset "1234567890")
					(mypunctset "!@#$")
					(charlength 10)
					(numlength 4)
					(punctlength 3)
					(result '()))
			(dotimes (i charlength)
				(setq result (cons (elt mycharset (random (length mycharset))) result)))
			(dotimes (i numlength)
				(setq result (cons (elt mynumset (random (length mynumset))) result)))
			(dotimes (i punctlength)
				(setq result (cons (elt mypunctset (random (length mypunctset))) result)))
			(concat (jea-make-pwrd--shuffle result))))

(defun jea-make-pwrd-clip-board()
	"Generate a password and store it on the clip board (aka `kill-ring`)
so the user can paste it wherever."
	(interactive)
	(random t) ;; seed random
	(kill-new (jea-make-pwrd-generate)))

(defun jea-make-pwrd-insert-buffer()
	"Generate a password and insert it into the current buffer."
  (interactive)
  (random t) ;; seed random
	(insert (jea-make-pwrd-generate)))

(provide 'jea-password-generate)

;;; jea-password-generate.el ends here
