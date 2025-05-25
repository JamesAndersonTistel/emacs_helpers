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

(defun jea--vc-get-code-char (in-code)
	"Convert IN-CODE to a number."
	(let ((result (alist-get (upcase in-code)
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

(defun jea--vc-get-shifted-char (plain-char key-char)
	"Get the vigenere shift for PLAIN-CHAR given KEY-CHAR."
	(if (string-equal plain-char " ")
			" "
		(let* ((key-shift (jea--vc-get-char-code key-char))
					 (plain-shift (jea--vc-get-char-code plain-char))
					 (result (mod (+ key-shift plain-shift) 26))) ; 26 letters in alphabet wrap
		(jea--vc-get-code-char result))))

(defun jea--vc-get-unshifted-char (plain-char key-char)
	"Get the vigenere shift for PLAIN-CHAR given KEY-CHAR."
	(if (string-equal plain-char " ")
			" "
		(let* ((key-shift (jea--vc-get-char-code key-char))
					 (plain-shift (jea--vc-get-char-code plain-char))
					 (result (mod (- plain-shift key-shift) 26))) ; 26 letters in alphabet wrap
		(jea--vc-get-code-char result))))

(defun jea--vigenere-run (plaintext key-str func)
	"Apply FUNC to do a vigenere cyper to PLAINTEXT with KEY-STR."
	(let* ((len-plain (length plaintext))
				 (pos-plain 0)
				 (len-key (length key-str))
				 (pos-key 0)
				 (result ""))
		(while (< pos-plain len-plain)
			(let* ((plain-char (substring plaintext pos-plain (1+ pos-plain)))
						 (key-char (substring key-str pos-key (1+ pos-key)))
						 (shift-char (funcall func plain-char key-char)))
				(message "JEA: %s %s %s %d %d" plain-char key-char shift-char pos-plain pos-key)
				(setq result (concat result shift-char))
				(setq pos-plain (1+ pos-plain))
				(setq pos-key (1+ pos-key))
				(setq pos-key (mod pos-key len-key))))
		result))

(defun jea-vigenere-encrypt (plaintext key-str)
	"Encrypt PLAINTEXT with KEY-STR."
	(jea--vigenere-run plaintext key-str 'jea--vc-get-shifted-char))

(defun jea-vigenere-decrypt (encrypted key-str)
	"Decrypt ENCRYPTED using KEY-STR."
	(jea--vigenere-run encrypted key-str 'jea--vc-get-unshifted-char))

(provide 'jea-vigenere-cipher)

;;; jea-vigenere-cipher.el ends here
