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

(defun jea-cg--py-preamble()
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

(defun jea-cg--py-variable-split(variable)
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

(defun jea-cg--py-variables-split(variables)
	"Convert VARIABLES into a name data type pairing."
	(let* ((expanded (mapcar #'jea-cg--py-variable-split variables)))
		(mapcar (lambda (x)
							(list (car x) (car (cdr x))))
						expanded)))

;; (jea-cg--py-variables-split '("ssleep" "ibark" "bdig"))
;; (("sleep" "str") ("bark" "int") ("dig" "bool"))

(defun jea-cg--py-variable-fmt(name type firstp lastp)
	"Format a variable with NAME of TYPE.
The FIRSTP and LASTP indicate first in list or last in list."
	(format "%s%s: %s%s" (if firstp "" " ") name type (if lastp "" ",")))

(defun jea-cg--py-ctor(name exp-vars)
	"Constructor boilerplate.  NAME is the class name.
EXP-VARS is the expanded arguments."
	(with-suppressed-warnings ()
		(let ((args (jea-code-gen-ctor-args-variables exp-vars 'jea-cg--py-variable-fmt))
					(vars (mapconcat
								 (lambda (v)
									 (format "        self._%s = %s\n" (car v) (car v)))
								 exp-vars)))
			(format "class %s:
    \"\"

    def __init__(self, %s):
%s

" (capitalize name) args vars))))

;; (jea-cg--py-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool")))

(defun jea-cg--py-func(name &optional exp-args)
	"Function boilerplate set to NAME with optional EXP-ARGS."
	(with-suppressed-warnings ()
		(let* ((arg-line (jea-code-gen-ctor-args-variables exp-args 'jea-cg--py-variable-fmt))
					 (params (if args (concat "self, " arg-line)
										"self")))
			(format "    def %s(%s):
        \"\"
        result = None
        return result

" name params))))

(defun jea-cg--py-add--compare(val other)
	"Return the correct comparison text between VAL and OTHER.
If NUM is non nil then its a number and alter the returned text."
	(let ((o (jea-string-get-print-format other))
				(result))
		(if (numberp o)
				(setq result (format "%s == %d" val o))
			(setq result (format "%s == '%s'" val o)))))

(defun jea-cg--py-switch(val cases)
	"Python does not have swtiches so this will make if/else.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(with-suppressed-warnings ()
		(let* ((result (format "    if %s:\n        pass\n"
													 (jea-cg--py-add--compare val (car cases)))))
			(dolist (case (cdr cases))
				(setq result (concat result (format "    elif %s:\n        pass\n"
																						(jea-cg--py-add--compare val case)))))
			(setq result (concat result "    else:\n        pass\n"))
			result)))

(defun jea-cg--py-dict-start-fmt(name)
	"Provided the start of a dict named NAME."
	(format "%s = {\n" name))

(defun jea-cg--py-dict-kv-fmt(key value firstp lastp)
	"Produce a key value pair appropriate to the language.
Based on KEY and VALUE.  FIRSTP will be true if its the first item.
LASTP will be true if it is the last item."
	(let ((v (jea-string-get-print-format value))) ; convert str "3.5" to 3.5 if number
		(if (numberp v)
				(format "    '%s': %s%s\n" key v (if lastp "" ","))
			(format "    '%s': '%s'%s\n" key v (if lastp "" ",")))))

;; (jea-cg--py-dict-kv-fmt "foo" "bar" nil nil)
;; (jea-cg--py-dict-kv-fmt "foo" 14 nil nil)
;; (jea-cg--py-dict-kv-fmt "bar" 14 nil t)
;; (jea-cg--py-dict-kv-fmt "foo" 15.4 nil t)

(defun jea-cg--py-dict-end-fmt()
	"Provided the end of a dict."
	(format "}\n"))

(defun jea-cg--py-dict(name kvs)
	"Produce a dict named NAME with key value pairs KVS."
	(jea-code-gen--build-dict name kvs
														'jea-cg--py-dict-start-fmt
														'jea-cg--py-dict-kv-fmt
														'jea-cg--py-dict-end-fmt))

;; (jea-cg--py-dict "d1" '("one" "2" "three" "four" "five" "6"))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

(defun jea-cg--py-insert-class (name variables)
	"Generate a class named NAME with the functions in the string VARIABLES.
VARIABLES will look like (\"ibark\", \"bjump\", \"sskip.\")"
	(let ((exp-vars (jea-cg--py-variables-split variables)))
		(insert (jea-cg--py-preamble))
		(insert (jea-cg--py-ctor name exp-vars))))

(defun jea-cg--py-insert-func (name args)
	"Generate a function named NAME with the args from ARGS.
AGRS will look like (\"bark\", \"jump\", \"skip.\")"
	(let ((exp-args (jea-cg--py-variables-split args)))
		(insert (jea-cg--py-func name exp-args))))

(defun jea-cg--py-insert-swtich (val cases)
	"Insert a swtich statment.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(insert (jea-cg--py-switch val cases)))

(defun jea-cg--py-insert-dict (val kvs)
	"Insert a dictionary statment.
VAL is the value that will be compared against.
KVS are the key value pairs."
	(insert (jea-cg--py-dict val cases)))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

;; (defun jea-test-run()
;;  	"Hook up F5 to run."
;;  	(interactive)
;;  	(with-current-buffer (get-buffer-create "*jea-code-gen*")
;;  		(erase-buffer)
;;  		;;(jea-cg--py-insert-class "dog" '("ssleepd" "ibark" "bdig" "sswim"))))
;; 		(jea-cg--py-insert-func "get_dog" '("ssleep" "ibark" "bdig" "sswim" "fscratch"))))
;;
;;  (global-set-key [(f5)] 'jea-test-run)

;; 	 ;;	(jea-code-gen--func-python "dog" '("sleep" "bark" "dig" "swim")))
;; 	(jea-cg--py-insert-swtich "x" '("1" "2" "3")))
;; ;; (jea-cg--py-insert-swtich "x" '("dog" "cat" "mouse")))


(defun jea-code-gen-use-python()
	"Turn on python code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func 'jea-cg--py-insert-class)
	(setf jea-code-gen-make-func-func 'jea-cg--py-insert-func)
	(setf jea-code-gen-make-switch-func 'jea-cg--py-insert-swtich)
	(setf jea-code-gen-make-dict-func 'jea-cg--py-insert-swtich)
	t)

(provide 'jea-code-gen-python)

;;; jea-code-gen-python.el ends here
