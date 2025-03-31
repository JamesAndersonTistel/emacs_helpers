;;; jea-code-gen-react.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler late code for react.

;;; Code:

(require 'jea-code-gen)
(require 'jea-string-util)

(defun jea-code-gen--react-preamble()
	"Start of file preamble text."
	(with-suppressed-warnings ()
		(format "// Copyright © %s James Anderson
//
// Author: James Anderson <james@tisteltech.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files \\(the \"Software\"\\), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so.
//
// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

" (format-time-string "%Y" (current-time)))))

(defun jea-cg--react-variable-split(variable)
	"Convert VARIABLE short code to long form.
\"isleep\" => \"sleep\" \"int\""

	(let* ((vtype (substring variable 0 1))
				 (result (alist-get vtype
														'(("i" . "int")
															("f" . "float")
															("b" . "bool")
															("s" . "str"))
														"str" nil 'string-equal))
				 (name (substring variable 1)))
		(list name result)))

(defun jea-cg--react-variables-split(variables)
	"Convert VARIABLES into a name data type pairing."
	(let* ((expanded (mapcar #'jea-cg--react-variable-split variables)))
		(mapcar (lambda (x)
							(list (car x) (car (cdr x))))
						expanded)))

(defun jea-cg--react-class-start(name)
	"Put in the opening text for a react class/function.
NAME is the function name."
	(format "import { useState } from \"react\";

export default function %s() {\n" (capitalize name)))

(defun jea-cg--react-default-val-use-state(vtype)
	"Get the default useState value for variables of type VTYPE."

	(let* ((result (alist-get vtype
														'(("int" . 0)
															("float" . 0.0)
															("bool" . "false")
															("str" . "\"\"")
															("string" . "\"\""))
														"null" nil 'string-equal)))
		result))

(defun jea-cg--react-build-use-state(name type)
	"Produce the right useState for the variable named NAME with type TYPE."
	(format "    const [%s, set%s] = useState(%s);\n" name
					(jea-capitalize-first name) (jea-cg--react-default-val-use-state type)))

(defun jea-cg--react-class-middle(exp-args)
	"Put in the middle text for a react class/function.
The middle text might have a variable number of elements for the
useState.  It might also have none.
EXP-ARGS is the expanded argument items to be put into useState."
	(let ((result ""))
		(dolist (ea exp-args)
			(setq result (concat result (jea-cg--react-build-use-state
																	 (car ea) (car (cdr ea))))))
		result))

(defun jea-cg--react-class-end()
	"Put in the closing text for a react class/function."
	(format "\n    return (<></>);\n}"))

(defun jea-cg--react-class(name exp-args)
	"Put in the text for a react class/function.
NAME is the class/function name.
EXP-ARGS is the expanded argument items to be put into useState."
	(concat (jea-code-gen--react-preamble)
					(jea-cg--react-class-start name)
					(jea-cg--react-class-middle exp-args)
					(jea-cg--react-class-end)))

(defun jea-cg--react-insert-class(name args)
	"Insert at poinr the react class/function named NAME with useState set to ARGS."
	(let ((exp-args (jea-cg--react-variables-split args)))
		(insert (jea-cg--react-class name exp-args))))

;; (defun jea-test-run()
;;  	"Hook up F5 to run."
;;  	(interactive)
;;  	(with-current-buffer (get-buffer-create "*jea-code-gen*")
;;  		(erase-buffer)
;; 		(jea-cg--react-insert-class "game" '("ibark" "bdigDown" "sswim" "fscratch"))))

;;  (global-set-key [(f5)] 'jea-test-run)

	
(defun jea-code-gen-use-react()
	"Turn on react code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func
				'jea-cg--react-insert-class)
	(setf jea-code-gen-make-func-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-switch-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-dict-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-api-get-func
				'(lambda (name functions) (message "not implemented yet.")))
	t)

(provide 'jea-code-gen-react)

;;; jea-code-gen-react.el ends here
