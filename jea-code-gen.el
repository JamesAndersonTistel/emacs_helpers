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

(defun jea-code-gen-class (name functions)
	"Generate a class named NAME with the functions in the string FUNCTIONS.

FUNCTIONS will look like \"bark, jump, skip.\""
	(funcall jea-code-gen-make-class-func name functions))

;;; functions
(defvar jea-code-gen-make-func-func
	'(lambda (name functions) (message "not implemented yet."))
	"Function to generate a function.")

(defun jea-code-gen-func (name args)
	"Generate a function named NAME with the ARGS.

FUNCTIONS will look like \"bark, jump, skip.\""
	(funcall jea-code-gen-make-func-func name args))

;;; switches
(defvar jea-code-gen-make-switch-func
	'(lambda (val cases) (message "not implemented yet."))
	"Function to generate a function.")

(defun jea-code-gen-switch (val cases)
	"Generate a switch statement.
VAL as value and CASES to compare against.

VAL will look like \"x\" and CASES like \"1, 2, 3.\""
	(funcall jea-code-gen-make-switch-func val cases))


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
			(jea-code-gen-class (car rest) (cdr rest)))
		 ((equal "func" command)
			(jea-code-gen-func (car rest) (cdr rest)))
		 (t
			(message "jea-code-gen-prompt unknown command: \"%s\"." command)))))

;; (jea-code-gen-python)

(provide 'jea-code-gen)

;;; jea-code-gen.el ends here
