;;; jea-code-gen-javascript.el --- code to break long strings into 280 character chunks

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

;;; Generate boiler late code for javascript.

;;; Code:

(require 'jea-code-gen)
(require 'jea-string-util)

(defun jea-code-gen--javascript-preamble()
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

(defun jea-cg--js-dict-start-fmt(name)
	"Provided the start of a dict named NAME."
	(format "let %s = {\n" name))

(defun jea-cg--js-dict-kv-fmt(key value firstp lastp)
	"Produce a key value pair appropriate to the language.
Based on KEY and VALUE.  FIRSTP will be true if its the first item.
LASTP will be true if it is the last item."
	(let ((v (jea-string-get-print-format value))) ; convert str "3.5" to 3.5 if number
		(if (numberp v)
				(format "    %s: %s%s\n" key v (if lastp "" ","))
			(format "    %s: '%s'%s\n" key v (if lastp "" ",")))))

(defun jea-cg--js-dict-end-fmt()
	"Provided the end of a dict."
	(format "};\n"))

(defun jea-cg--js-dict(name kvs)
	"Produce a dict named NAME with key value pairs KVS."
	(jea-code-gen--build-dict name kvs
														'jea-cg--js-dict-start-fmt
														'jea-cg--js-dict-kv-fmt
														'jea-cg--js-dict-end-fmt))

(defun jea-cg--js-add--compare(val other)
	"Return the correct comparison text between VAL and OTHER.
If NUM is non nil then its a number and alter the returned text."
	(let ((o (jea-string-get-print-format other))
				(result))
		(if (numberp o)
				(setq result (format "case %d:" o))
			(setq result (format "case '%s':" o)))
	result))

(defun jea-cg--js-switch(val cases)
	"Python does not have swtiches so this will make if/else.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(with-suppressed-warnings ()
		(let* ((result (format "    switch(%s) {\n" val)))
			(dolist (case cases)
				(setq result (concat result (format "    %s\n        break;\n"
																						(jea-cg--js-add--compare val case)))))
			(setq result (concat result "    default:\n        break;\n    }"))
			result)))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

(defun jea-cg--js-insert-dict (val kvs)
	"Insert a dictionary statment.
VAL is the value that will be compared against.
KVS are the key value pairs."
	(insert (jea-cg--js-dict val kvs)))

(defun jea-cg--js-insert-swtich (val cases)
	"Insert a swtich statment.
VAL is the value that will be compared against.
CASES are the values that will be compared to VAL."
	(insert (jea-cg--js-switch val cases)))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

(defun jea-code-gen-use-javascript()
	"Turn on python code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-func-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-switch-func 'jea-cg--js-insert-swtich)
	(setf jea-code-gen-make-dict-func 'jea-cg--js-insert-dict)
	(setf jea-code-gen-make-api-get-func
				'(lambda (name functions) (message "not implemented yet.")))
	t)

(provide 'jea-code-gen-javascript)

;;; jea-code-gen-javascript.el ends here
