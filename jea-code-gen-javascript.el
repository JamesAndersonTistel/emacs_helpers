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
		"// Copyright © 2025 James Anderson
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

"))

(defun jea-cg--js-dict-start-fmt(name)
	"Provided the start of a dict named NAME."
	(format "let %s = {\n" name))

(defun jea-cg--js-dict-kv-fmt(key value firstp lastp)
	"Produce a key value pair appropriate to the language.
Based on KEY and VALUE.  FIRSTP will be true if its the first item.
LASTP will be true if it is the last item."
	(let ((v (jea-string-get-print-format value))) ; convert str "3.5" to 3.5 if number
		(if (numberp v)
				(format "    '%s': %s%s\n" key v (if lastp "" ","))
			(format "    '%s': '%s'%s\n" key v (if lastp "" ",")))))

;; (jea-cg--js-dict-kv-fmt "foo" "bar" nil nil)
;; (jea-cg--js-dict-kv-fmt "foo" 14 nil nil)
;; (jea-cg--js-dict-kv-fmt "bar" 14 nil t)
;; (jea-cg--js-dict-kv-fmt "foo" 15.4 nil t)

(defun jea-cg--js-dict-end-fmt()
	"Provided the end of a dict."
	(format "};\n"))

(defun jea-cg--js-dict(name kvs)
	"Produce a dict named NAME with key value pairs KVS."
	(jea-code-gen--build-dict name kvs
														'jea-cg--js-dict-start-fmt
														'jea-cg--js-dict-kv-fmt
														'jea-cg--js-dict-end-fmt))

;; (jea-cg--js-dict "d1" '("one" "2" "three" "four" "five" "6"))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

(defun jea-cg--js-insert-dict (val kvs)
	"Insert a dictionary statment.
VAL is the value that will be compared against.
KVS are the key value pairs."
	(insert (jea-cg--js-dict val kvs)))

;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------

(defun jea-code-gen-use-javascript()
	"Turn on python code gen.  Set local funcs to the global vars."
	(interactive)
	(setf jea-code-gen-make-class-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-func-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-switch-func
				'(lambda (name functions) (message "not implemented yet.")))
	(setf jea-code-gen-make-dict-func 'jea-cg--js-insert-dict)
	t)

(provide 'jea-code-gen-javascript)

;;; jea-code-gen-javascript.el ends here
