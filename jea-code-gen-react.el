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

(defun jea-code-gen-use-react()
	"Turn on react code gen.  Set local funcs to the global vars."
	(interactive)
	;;(setf jea-code-gen-make-class-func 'jea-code-gen--insert-class-react)
	;;(setf jea-code-gen-make-func-func 'jea-code-gen--insert-func-react)
	t)

(provide 'jea-code-gen-react)

;;; jea-code-gen-react.el ends here
