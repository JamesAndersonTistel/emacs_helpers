;;; jea-code-gen-elixir.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler late code for elixir.

;;; Code:

(require 'jea-code-gen)
(require 'jea-string-util)

(defun jea-code-gen--elixir-preamble()
	"Start of file preamble text."
	(with-suppressed-warnings ()
		"# Copyright © 2025 James Anderson
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
"))

(defun jea-code-gen--elixir-ctor(name)
	"Constructor boilerplate.  NAME is the class name."
	(with-suppressed-warnings ()
		(format "class %s:
    \"\"

    def __init__(self):
        pass

" (capitalize name))))

(defun jea-code-gen--elixir-func(name &optional args)
	"Function boilerplate set to NAME with optional ARGS."
	(with-suppressed-warnings ()
		(let ((params (if args (concat "self, " (string-join args ", "))
										"self")))
			(format "    def %s(%s):
        \"\"
        result = None
        return result

" name params))))

(defun jea-code-gen--insert-class-elixir (name functions)
	"Generate a class named NAME with the functions in the string FUNCTIONS.
FUNCTIONS will look like (\"bark\", \"jump\", \"skip.\")"
	(insert (jea-code-gen--elixir-preamble))
	(insert (jea-code-gen--elixir-ctor name))
	(dolist (f functions)
		(insert (jea-code-gen--elixir-func f nil))))

(defun jea-code-gen--insert-func-elixir (name args)
	"Generate a function named NAME with the args from ARGS.
AGRS will look like (\"bark\", \"jump\", \"skip.\")"
	(insert (jea-code-gen--elixir-func name args)))

;; (with-current-buffer (get-buffer-create "*jea-code-gen*")
;;   (erase-buffer)
;; ;;   (jea-code-gen-class "dog" '("sleep" "bark" "dig" "swim")))
;; 	(jea-code-gen--func-elixir "dog" '("sleep" "bark" "dig" "swim")))

(defun jea-code-gen-elixir()
	"Turn on elixir code gen.  Set local funcs to the global vars."
	(setf jea-code-gen-make-class-func 'jea-code-gen--insert-class-elixir)
	(setf jea-code-gen-make-func-func 'jea-code-gen--insert-func-elixir)
	t)

(provide 'jea-code-gen-elixir)

;;; jea-code-gen-elixir.el ends here
