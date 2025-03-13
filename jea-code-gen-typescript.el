;;; jea-code-gen-typescript.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler late code for typescript.

;;; Code:

(require 'jea-code-gen)
(require 'jea-string-util)

(defun jea-code-gen--typescript-preamble()
	"Start of file preamble text."
	(with-suppressed-warnings ()
		(format "/*
Copyright © %s James Anderson

Author: James Anderson <james@tisteltech.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files \\(the \"Software\"\\), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

" (format-time-string "%Y" (current-time)))))

(defun jea-variable--split(variable)
	"Convert VARIABLE short code to long form.
\"nsleep\" => \"sleep\" \"number\""

	(let* ((vtype (substring variable 0 1))
				 (result (alist-get vtype
														 '(("s" . "string")
															 ("n" . "number")
															 ("b" . "boolean"))
														 "not found" nil 'string-equal))
				 (name (substring variable 1)))
		(list name result)))

(defun jea-variables--split(variables)
	"Convert VARIABLES into a name data type pairing."
	(let* ((expanded (mapcar #'jea-variable--split variables)))
		(mapcar (lambda (x)
							(format "%s: %s" (car x) (car (cdr x))))
						expanded)))

;; (jea-variables--split '("ssleep" "nbark" "bdig"))
;; ("sleep: string" "bark: number" "dig: boolean")

(defun jea-expand-declaration-variables(expanded-vars)
	"Produce text that can be used as variable declaration.
Use EXPANDED-VARS to get the values."
	(mapconcat (lambda (v)
							 (format "    /**
    *
    */
    %s;

" v)) expanded-vars))

;; (jea-expand-declaration-variables '("sleep: string" "bark: number" "dig: boolean"))

(defun jea-expand-ctor-variables(expanded-vars)
	"Produce code that is suitable for a class constructor.
Use EXPANDED-VARS to get the vlaues. We need to behave differently on the last one."
	(let ((count 0)
				(max (- (length expanded-vars) 1))
				(result ""))
		(dolist (v expanded-vars)
			(if (< count max)
					(progn
						(setq result (concat result (format "%s, " v)))
						(setq count (1+ count)))
				(setq result (concat result (format "%s" v)))))
		result))

(jea-expand-ctor-variables '("sleep: string" "bark: number" "dig: boolean"))

(defun jea-code-gen--typescript-class(name variables)
	"Constructor boilerplate.  NAME is the class name.
VARIABLES will be expanded into class variables.  Also functions
will be generated even if not often used.  Its easy to delete."
	(with-suppressed-warnings ()
		(format "
/**
 *
 */
class %s {

" (capitalize name))))

(defun jea-code-gen--typescript-func(name &optional args)
	"Function boilerplate set to NAME with optional ARGS."
	(with-suppressed-warnings ()
		(let ((params (if args (concat "self, " (string-join args ", "))
										"self")))
			(format "    def %s(%s):
        \"\"
        result = None
        return result

" name params))))

(defun jea-code-gen--insert-class-typescript (name functions)
	"Generate a class named NAME with the functions in the string FUNCTIONS.
FUNCTIONS will look like (\"bark\", \"jump\", \"skip.\")"
	;; (insert (jea-code-gen--typescript-preamble))
	(insert (jea-code-gen--typescript-ctor name))
	(dolist (f functions)
		(insert (jea-code-gen--typescript-func f nil))))

(defun jea-code-gen--insert-func-typescript (name args)
	"Generate a function named NAME with the args from ARGS.
AGRS will look like (\"bark\", \"jump\", \"skip.\")"
	(insert (jea-code-gen--typescript-func name args)))

(defun jea-code-gen-use-typescript()
	"Turn on typescript code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func 'jea-code-gen--insert-class-typescript)
	t)

;; use this to easily test large text ouputs
(defun jea-test-run()
	"Hook up F5 to run."
	(interactive)
	(with-current-buffer (get-buffer-create "*jea-code-gen*")
		(erase-buffer)
		(jea-code-gen--insert-class-typescript "dog" '("ssleep" "nbark" "bdig"))
		))

(global-set-key [(f5)] 'jea-test-run)

(provide 'jea-code-gen-typescript)

;;; jea-code-gen-typescript.el ends here
