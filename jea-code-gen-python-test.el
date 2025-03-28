;;; jea-code-gen-python-test.el --- test the python code gen

;;; Code:

(require 'jea-code-gen-python)
(require 'jea-code-gen-test)

;; need to setup a proper unit test system soon
;; just moving the test code out
;; START here need to be more systematic and bring over each func.
;; pass in debug as a arg with default off

(defun jea-code-gen-test-python()
	"Test all python functions."
	(let ((t1 (jea-test-text '(lambda () (jea-cg--py-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool"))))
"class Cat:
    \"\"

    def __init__(self, sleep: str, bark: int, dig: bool):
        self._sleep = sleep
        self._bark = bark
        self._dig = dig


"))
				(t2 (jea-test-list '(lambda () (jea-cg--py-variables-split '("ssleep" "ibark" "bdig")))
													 '(("sleep" "str") ("bark" "int") ("dig" "bool")))))
		(and t1 t2)))

;; (jea-code-gen-test-python)

;; (jea-cg--py-dict-kv-fmt "foo" "bar" nil nil)
;; (jea-cg--py-dict-kv-fmt "foo" 14 nil nil)
;; (jea-cg--py-dict-kv-fmt "bar" 14 nil t)
;; (jea-cg--py-dict-kv-fmt "foo" 15.4 nil t)

;; (jea-cg--py-dict "d1" '("one" "2" "three" "four" "five" "6"))

(provide 'jea-code-gen-python-test)

;;; jea-code-gen-python-test.el ends here
