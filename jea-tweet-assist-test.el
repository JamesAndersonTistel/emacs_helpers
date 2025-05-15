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
(require 'jea-code-gen-test)

;; --------------------------------------------------------------------------------
;; utility functions

(defun jea--get-test-input01()
	"Example input test one."
	" Lorem ipsum dolor sit amet, consectetur \"adipiscing\" elit. Proin et quam a dolor maximus facilisis et vel sem. Vestibulum ullamcorper ut nisl in fringilla. Vestibulum nec commodo tortor. Nulla sit amet tellus fringilla, tristique tortor in, aliquet dolor. Integer iaculis velit sit amet dui mollis semper. Ut tincidunt nisl ac tortor viverra laoreet. Donec euismod velit malesuada consequat faucibus.

Sed eget aliquam arcu. Nunc sit amet diam sed felis elementum tempus eget sed metus. Suspendisse potenti. Praesent porta augue id eros facilisis lobortis. Suspendisse vestibulum quis ante in facilisis. Cras nec rutrum tortor. Maecenas vestibulum nisi vel orci interdum porta. Morbi eu urna pulvinar, pellentesque massa id, sagittis nisi. Donec vulputate, arcu quis aliquet dapibus, augue quam vulputate lectus, in pellentesque enim felis ut sem. Aliquam vestibulum, mi quis malesuada euismod, lectus lacus sagittis magna, eu interdum magna dolor vel turpis. Sed varius tellus ut sapien faucibus, faucibus commodo mi rhoncus. Fusce non varius lacus, luctus ultricies felis. Maecenas nec sagittis purus.

Etiam purus ipsum, lobortis vel convallis sit amet, eleifend ut magna. Aenean consequat feugiat facilisis. Ut nibh quam, finibus eu aliquet sagittis, rutrum a purus. Suspendisse ut ipsum sit amet sapien accumsan molestie eget quis enim. Suspendisse hendrerit vel mauris in convallis. Nullam rhoncus magna sem. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Maecenas nec rhoncus magna. Aenean ligula ligula, imperdiet non urna nec, consectetur mattis nisl.

Aenean laoreet fringilla mi id ullamcorper. Etiam suscipit mauris eu nisl convallis tristique. Duis tincidunt quis neque eu dapibus. Fusce scelerisque ex varius ex viverra, eu accumsan eros egestas. Nullam placerat vitae est nec iaculis. Pellentesque dapibus purus ac orci auctor posuere. Integer commodo velit sit amet risus commodo congue.
")

(defun jea--get-test-output01()
	"Example output test one."
	"Lorem ipsum dolor sit amet, consectetur \"adipiscing\" elit. Proin et quam a dolor maximus facilisis et vel sem. Vestibulum ullamcorper ut nisl in fringilla. Vestibulum nec commodo tortor. Nulla sit amet tellus fringilla, tristique tortor in, aliquet dolor. 🧵1/9

Integer iaculis velit sit amet dui mollis semper. Ut tincidunt nisl ac tortor viverra laoreet. Donec euismod velit malesuada consequat faucibus. Sed eget aliquam arcu. Nunc sit amet diam sed felis elementum tempus eget sed metus. Suspendisse potenti. 🧵2/9

Praesent porta augue id eros facilisis lobortis. Suspendisse vestibulum quis ante in facilisis. Cras nec rutrum tortor. Maecenas vestibulum nisi vel orci interdum porta. Morbi eu urna pulvinar, pellentesque massa id, sagittis nisi. 🧵3/9

Donec vulputate, arcu quis aliquet dapibus, augue quam vulputate lectus, in pellentesque enim felis ut sem. Aliquam vestibulum, mi quis malesuada euismod, lectus lacus sagittis magna, eu interdum magna dolor vel turpis. 🧵4/9

Sed varius tellus ut sapien faucibus, faucibus commodo mi rhoncus. Fusce non varius lacus, luctus ultricies felis. Maecenas nec sagittis purus. Etiam purus ipsum, lobortis vel convallis sit amet, eleifend ut magna. Aenean consequat feugiat facilisis. 🧵5/9

Ut nibh quam, finibus eu aliquet sagittis, rutrum a purus. Suspendisse ut ipsum sit amet sapien accumsan molestie eget quis enim. Suspendisse hendrerit vel mauris in convallis. Nullam rhoncus magna sem. 🧵6/9

Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Maecenas nec rhoncus magna. Aenean ligula ligula, imperdiet non urna nec, consectetur mattis nisl. Aenean laoreet fringilla mi id ullamcorper. 🧵7/9

Etiam suscipit mauris eu nisl convallis tristique. Duis tincidunt quis neque eu dapibus. Fusce scelerisque ex varius ex viverra, eu accumsan eros egestas. Nullam placerat vitae est nec iaculis. Pellentesque dapibus purus ac orci auctor posuere. 🧵8/9

Integer commodo velit sit amet risus commodo congue. 🧵9/9

")

;; --------------------------------------------------------------------------------

(defun jea--tweet-assist-test-set1()
	"We need to make sure when we split a long social media post that we try to preserve un-split-able things."
	(let ((t1 (jea-test-list '(lambda ()
															(jea-tweet--split "foo https://www.example.com/stories/usnavy/357/ bar"))
													 '("foo " "https://www.example.com/stories/usnavy/357/" " bar")))
				(t2 (jea-test-list '(lambda ()
															(jea-tweet--split "https://www.example.com/stories/usnavy/35731/"))
													 '("https://www.example.com/stories/usnavy/35731/")))
				(t3 (jea-test-list '(lambda ()
															(jea-tweet--split "https://www.example.com/stories/usnavy/35731714/ bar"))
													 '("https://www.example.com/stories/usnavy/35731714/" " bar")))
				(t4 (jea-test-list '(lambda ()
															(jea-tweet--split "foo bar"))
													 '("foo bar")))
				)
		(and t1 t2 t3 t4)))

;; --------------------------------------------------------------------------------

(defun jea--tweet-assist-test-set2()
	"Text split into sentences."
	(let ((t1 (jea-test-lambda '(lambda ()
																(nth 1 (jea-tweet--split-into-sentences (jea-test-get-long-random-text1))))
														 '(lambda (data)
																(stringp data))))
				(t2 (jea-test-lambda '(lambda ()
																(jea-tweet--split-into-sentences (jea-test-get-long-random-text1)))
														 '(lambda (data)
																(> (length data) 1)))))
		(and t1 t2)))

;; --------------------------------------------------------------------------------

(defun jea--tweet-assist-test()
	"Run the test."
	(and (jea--tweet-assist-test-set1)
			 (jea--tweet-assist-test-set2)
			 (equal (jea-tweet-split-long (jea--get-test-input01)) (jea--get-test-output01))))

(jea--tweet-assist-test)

;; ugh, not splitting quote might be tricky, there could be several
;; (jea-find-string-all "This string contains \"quotes are tricky\" that are fun to with." "\"\\([a-z A-Z]+\\)\"")
;; (jea-find-string-all "This string contains \"quotes are tricky\" that are fun to \"with\"." "\"\\([a-z A-Z]+\\)\"")

(defun jea-test-run()
  "Hook up F5 to run."
  (interactive)
  (with-current-buffer (get-buffer-create "*jea-code-gen*")
  	(erase-buffer)
  	(message "tweet test: %s." (jea--tweet-assist-test))))

(global-set-key [(f5)] 'jea-test-run)

;;; jea-tweet-assist-test.el ends here
