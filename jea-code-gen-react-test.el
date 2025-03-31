;;; jea-code-gen-react-test.el --- test the react code gen

;;; Code:

(require 'jea-code-gen-react)
(require 'jea-code-gen-test)

;; need to setup a proper unit test system soon
;; just moving the test code out
;; START here need to be more systematic and bring over each func.
;; pass in debug as a arg with default off

(defun jea-code-gen-test-react()
	"Test all react functions."
	(let ((t1 (jea-test-text '(lambda ()
															(jea-cg--js-dict "d1"
																							 '("one" "2" "three" "four" "five" "6")))
"let d1 = {
    one: 2,
    three: 'four',
    five: 6
};
"))
				(t2 (jea-test-text '(lambda ()
															(jea-cg--js-switch "pet" '("dog" "cat" "fish")))
"    switch(pet) {
    case 'dog':
        break;
    case 'cat':
        break;
    case 'fish':
        break;
    default:
        break;
    }"))
				(t3 (jea-test-text '(lambda ()
															(jea-cg--js-switch "petCount" '(1 2 3)))
"    switch(petCount) {
    case 1:
        break;
    case 2:
        break;
    case 3:
        break;
    default:
        break;
    }")))
		(and t1 t2 t3)))

(jea-code-gen-test-react)

;; right now this is just a copy of the python one.

(provide 'jea-code-gen-react-test)

;;; jea-code-gen-react-test.el ends here
