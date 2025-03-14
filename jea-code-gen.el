;;; jea-code-gen.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler plate code (class, switch etc) for various
;;; languages.  When each language is turned on, it will set the
;;; defvars here.  This way we can have shortcuts pointed here that
;;; don't need to change.

;;; Code:

;;; Classes
(defvar jea-code-gen-make-class-func
	'(lambda (name functions) (message "not implemented yet."))
	"Function to generate a class.")

;;; Functions
(defvar jea-code-gen-make-func-func
	'(lambda (name functions) (message "not implemented yet."))
	"Function to generate a function.")

;;; Switches
(defvar jea-code-gen-make-switch-func
	'(lambda (val cases) (message "not implemented yet."))
	"Function to generate a function.")

(defun jea-code-gen-prompt (arg command)
	"ARG is prefix argument.  COMMAND is the code gen command.
For example: class,dog,sleep,bark,dig,swim.  Will insert a class
called Dog with the functions: sleep, bark, dig, swim."
	(interactive "p\nsWhat code to gen: ")
	(let* ((split (string-split command ","))
				 (command (car split))
				 (rest (cdr split)))
		(cond
		 ((equal "class" command)
			(funcall jea-code-gen-make-class-func (car rest) (cdr rest))) ;; name functions
		 ((equal "func" command)
			(funcall jea-code-gen-make-func-func (car rest) (cdr rest))) ;; name args
		 ((equal "switch" command)
			(funcall jea-code-gen-make-switch-func (car rest) (cdr rest))) ;; val cases
		 (t
			(message "jea-code-gen-prompt unknown command: \"%s\"." command)))))

;; (jea-code-gen-use-python)
;; (jea-code-gen-use-typescript)
;; (jea-code-gen-use-react)

(provide 'jea-code-gen)

;;; jea-code-gen.el ends here
