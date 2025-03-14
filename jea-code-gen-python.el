;;; jea-code-gen-python.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler late code for python3.

;;; Code:

(require 'jea-code-gen)
(require 'jea-string-util)

(defun jea-code-gen--python-preamble()
	"Start of file preamble text."
	(with-suppressed-warnings ()
		(format "# Copyright © %s James Anderson
#
# Author: James Anderson <james@tisteltech.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files \\(the \"Software\"\\), to deal in the Software
# without restriction, including without limitation the rights to use, copy, modify,
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so.
#
# THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

\"\"\"
\"\"\"
" (format-time-string "%Y" (current-time)))))

(defun jea-code-gen--python-variable-split(variable)
	"Convert VARIABLE short code to long form.
\"isleep\" => \"sleep\" \"int\""

	(let* ((vtype (substring variable 0 1))
				 (result (alist-get vtype
														 '(("i" . "int")
															 ("f" . "float")
															 ("b" . "bool")
															 ("s" . "str"))
														 "not found" nil 'string-equal))
				 (name (substring variable 1)))
		(list name result)))

(defun jea-code-gen--python-variables-split(variables)
	"Convert VARIABLES into a name data type pairing."
	(let* ((expanded (mapcar #'jea-code-gen--python-variable-split variables)))
		(mapcar (lambda (x)
							(list (car x) (car (cdr x))))
						expanded)))

;; (jea-code-gen--python-variables-split '("ssleep" "ibark" "bdig"))
;; (("sleep" "str") ("bark" "int") ("dig" "bool"))

(defun jea-code-gen--python-variable-fmt(name type firstp lastp)
	"Format a variable with NAME of TYPE.
The FIRSTP and LASTP indicate first in list or last in list."
	(format "%s%s: %s%s" (if firstp "" " ") name type (if lastp "" ",")))

(defun jea-code-gen--python-ctor(name exp-vars)
	"Constructor boilerplate.  NAME is the class name.
EXP-VARS is the expanded arguments."
	(with-suppressed-warnings ()
		(let ((args (jea-code-gen-ctor-args-variables exp-vars 'jea-code-gen--python-variable-fmt))
					(vars (mapconcat
								 (lambda (v)
									 (format "        self._%s = %s;\n" (car v) (car v)))
								 exp-vars)))
			(format "class %s:
    \"\"

    def __init__(self, %s):
%s

" (capitalize name) args vars))))

;; (jea-code-gen--python-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool")))

;; START here, fix the func names to have correct prefix for lang

(defun jea-code-gen--python-func(name &optional args)
	"Function boilerplate set to NAME with optional ARGS."
	(with-suppressed-warnings ()
		(let ((params (if args (concat "self, " (string-join args ", "))
										"self")))
			(format "    def %s(%s):
        \"\"
        result = None
        return result

" name params))))

(defun jea-add--compare(val other)
	"Return the correct comparison text between VAL and OTHER.
If NUM is non nil then its a number and alter the returned text."
	(let ((o (jea-string-get-print-format other))
				(result))
		(if (numberp o)
				(setq result (format "%s == %d" val o))
			(setq result (format "%s == '%s'" val o)))))

(defun jea-code-gen--python-switch(val cases)
	"Python does not have swtiches so this will make if/else.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(with-suppressed-warnings ()
		(let* ((result (format "    if %s:\n        pass\n"
													 (jea-add--compare val (car cases)))))
			(dolist (case (cdr cases))
				(setq result (concat result (format "    elif %s:\n        pass\n"
																						(jea-add--compare val case)))))
			(setq result (concat result "    else:\n        pass\n"))
			result)))

(defun jea-code-gen--insert-class-python (name variables)
	"Generate a class named NAME with the functions in the string VARIABLES.
VARIABLES will look like (\"ibark\", \"bjump\", \"sskip.\")"
	;;(insert (jea-code-gen--python-preamble))
	(insert (jea-code-gen--python-ctor name variables)))

(defun jea-code-gen--insert-func-python (name args)
	"Generate a function named NAME with the args from ARGS.
AGRS will look like (\"bark\", \"jump\", \"skip.\")"
	(insert (jea-code-gen--python-func name args)))

(defun jea-code-gen--insert-swtich-python (val cases)
	"Insert a swtich statment.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(insert (jea-code-gen--python-switch val cases)))

(defun jea-test-run()
	"Hook up F5 to run."
	(interactive)
	(with-current-buffer (get-buffer-create "*jea-code-gen*")
		(erase-buffer)
		(jea-code-gen--insert-class-python "dog" '("ssleep" "ibark" "bdig" "sswim"))))

;; (global-set-key [(f5)] 'jea-test-run)

;; 	 ;;	(jea-code-gen--func-python "dog" '("sleep" "bark" "dig" "swim")))
;; 	(jea-code-gen--insert-swtich-python "x" '("1" "2" "3")))
;; ;; (jea-code-gen--insert-swtich-python "x" '("dog" "cat" "mouse")))


(defun jea-code-gen-use-python()
	"Turn on python code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func 'jea-code-gen--insert-class-python)
	(setf jea-code-gen-make-func-func 'jea-code-gen--insert-func-python)
	(setf jea-code-gen-make-switch-func 'jea-code-gen--insert-swtich-python)
	t)

(provide 'jea-code-gen-python)

;;; jea-code-gen-python.el ends here
