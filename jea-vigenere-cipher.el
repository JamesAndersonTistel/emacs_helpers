;;; jea-vigenere-cipher.el --- code to make a Vigenère cipher

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

(defun jea--vc-get-char-code (in-char)
	"Convert IN-CHAR to a number."
	(let ((result (alist-get (upcase in-char)
													 '(("A" . 0)
														 ("B" . 1)
														 ("C" . 2)
														 ("D" . 3)
														 ("E" . 4)
														 ("F" . 5)
														 ("G" . 6)
														 ("H" . 7)
														 ("I" . 8)
														 ("J" . 9)
														 ("K" . 10)
														 ("L" . 11)
														 ("M" . 12)
														 ("N" . 13)
														 ("O" . 14)
														 ("P" . 15)
														 ("Q" . 16)
														 ("R" . 17)
														 ("S" . 18)
														 ("T" . 19)
														 ("U" . 20)
														 ("V" . 21)
														 ("W" . 22)
														 ("X" . 23)
														 ("Y" . 24)
														 ("Z" . 25))
													 "???" nil 'string-equal)))
		result))

(defun jea--vc-get-code-char (in-char)
	"Convert IN-CHAR to a number."
	(let ((result (alist-get (upcase in-char)
													 '((0 . "A")
														 (1 . "B")
														 (2 . "C")
														 (3 . "D")
														 (4 . "E")
														 (5 . "F")
														 (6 . "G")
														 (7 . "H")
														 (8 . "I")
														 (9 . "J")
														 (10 . "K")
														 (11 . "L")
														 (12 . "M")
														 (13 . "N")
														 (14 . "O")
														 (15 . "P")
														 (16 . "Q")
														 (17 . "R")
														 (18 . "S")
														 (19 . "T")
														 (20 . "U")
														 (21 . "V")
														 (22 . "W")
														 (23 . "X")
														 (24 . "Y")
														 (25 . "Z"))
													 -1 nil 'eq)))
		result))

(provide 'jea-vigenere-cipher)

;;; jea-vigenere-cipher.el ends here
