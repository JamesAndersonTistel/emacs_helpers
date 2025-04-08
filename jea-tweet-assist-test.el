;;; jea-tweet-assist-test.el --- test the code to break long strings into 280 character chunks

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

;;; test code to break long strings into 280 character chunks
;;; this needs to be improved into proper unit tests (TODO)

;;; Code:

(require 'jea-tweet-assist)

(defun jea-tweet--test-match(a b)
	"Make sure they are the same thing and equal.
A the first argument to compare
B the second argument to compare"
	(cond ((and (stringp a) (stringp b)) (equal a b))
				((and (listp a) (listp b)) (equal a b))
				(t nil)))

(defun jea-tweet--make-error-msg-build(a)
	"Build for individual argument.

A is the argument"
	(if (listp a)
			(mapconcat 'identity a " ")
		a))

(defun jea-tweet--make-error-msg(a b)
	"Make an easy to read error message.

A the first argument to process
B the second argument to process"
	(concat "'"
					(jea-tweet--make-error-msg-build a)
					"' does not match: '"
					(jea-tweet--make-error-msg-build b) "'."))

(defun jea-tweet--test1-in-data()
	"Lorem ipsum odor amet, consectetuer adipiscing elit.")

(defun jea-tweet--test1-out-data()
	"Lorem ipsum odor amet, consectetuer adipiscing elit.")

(defun jea-tweet--test1()
	"Test small string that should not be modified."
	(interactive)
	(let* ((in-data (jea-tweet--test1-in-data))
				 (result (jea-tweet--split-long in-data))
				 (out-data (jea-tweet--test1-out-data))
				 (success (jea-tweet--test-match out-data result)))
		(if success
				t
			(message (jea-tweet--make-error-msg out-data result)))))

;; (jea-tweet--test1)


(defun jea-tweet--test2-in-data()
	"Long string test."
	(with-suppressed-warnings ()
		"Lorem ipsum odor amet, consectetuer adipiscing elit. Dolor nec magnis aliquet risus dapibus mi tempus lorem. Maximus ultrices faucibus varius lectus fames dictum sodales per. Eget nisi ridiculus eu fusce ornare nisl. Dui pellentesque aptent efficitur vulputate feugiat luctus enim. Inceptos dictum ultricies, mollis venenatis finibus donec habitasse. Vehicula malesuada tellus nec dis phasellus; sit varius felis diam. Lobortis rutrum faucibus lacus dapibus cubilia volutpat faucibus congue.

Sed orci fermentum nisl ipsum leo blandit. Nascetur eros hac bibendum orci laoreet etiam laoreet est tristique. Litora magnis dictum mi penatibus venenatis tincidunt eu etiam mattis. Eu potenti iaculis pulvinar venenatis tristique gravida placerat ad aenean. Dolor duis cursus malesuada praesent varius dis. Rutrum habitasse ligula curae vitae proin feugiat auctor mauris dolor. Malesuada rutrum morbi netus cursus natoque a cubilia condimentum non. Platea ante ipsum sociosqu curabitur pretium.

Donec ut habitasse fermentum dolor, conubia est. Habitasse urna metus arcu torquent consectetur leo. Magna vestibulum tellus montes auctor luctus sem condimentum at. Gravida dis nunc consequat mattis turpis vel condimentum dignissim. Magnis curae fames mi, dis quis cursus bibendum. Gravida semper ultricies platea nulla massa aptent. Netus non et; nam auctor mollis laoreet. Turpis eros enim; sollicitudin justo vivamus hac vivamus.

Phasellus nec elit, nullam dolor porta netus vulputate laoreet. Eu congue ultricies risus ultricies aenean scelerisque natoque. Natoque dictum id pharetra orci dui. Efficitur       faucibus ridiculus phasellus proin tortor litora. Nisi gravida euismod fringilla netus pellentesque vestibulum. Semper faucibus lobortis lacinia curabitur posuere habitasse dui dictum.

Tempor purus nisl nisl feugiat hendrerit eros convallis sit. Ac luctus eu penatibus ante porta rutrum diam fusce conubia. Conubia blandit convallis elementum rutrum ultricies vehicula. Fames congue vitae neque molestie vitae montes porttitor. Mauris facilisi urna quisque nostra dui; sociosqu duis ridiculus. Ornare felis ullamcorper praesent eu elementum phasellus suscipit dignissim sapien. Phasellus elit libero elementum venenatis blandit pellentesque amet. Congue habitant primis tempus leo congue fusce sagittis himenaeos feugiat."))

(defun jea-tweet--test2-out-data()
	"Test basic text split into three tweets."
	'("foo" "bar"))

(defun jea-tweet--test2()
	"Test basic text split into three tweets."
	(interactive)
	(let* ((in-data (jea-tweet--test2-in-data))
				 (result (jea-tweet--split-long in-data))
				 (out-data (jea-tweet--test2-out-data))
				 (success (jea-tweet--test-match out-data result)))
		(if success
				t
			(message (jea-tweet--make-error-msg out-data result)))))

;; (jea-tweet--test2)

;; makes it easy to hit F5 in another buffer to run current test
;; (global-set-key [(f5)] 'jea-tweet--test2)

;;; jea-tweet-assist-test.el ends here
