;;; jea-code-gen-python-test.el --- test the python code gen

;;; Code:

(require 'jea-code-gen-python)

;; need to setup a proper unit test system soon
;; just moving the test code out

;; TODO write a string diff thay high lights the difference! char by char.

(defmacro jea-test-text (func expected-text-output)
	"Run the function FUNC and double check the text output is EXPECTED-TEXT-OUTPUT."
	`(let* ((debug t) ;; turn on prints
					(output (funcall ,func))
					(success (string-equal output ,expected-text-output)))
		 (if success
				 (progn
					 (if debug
							 (message "success %s" ,func))
					 t)
			 (progn
				 (if debug
						 (message "failed because %s: '%s' does not equal '%s'." ,func output ,expected-text-output))
				 nil))))

(jea-test-text '(lambda () (jea-cg--py-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool"))))
"class Cat:
    \"\"

    def __init__(self, sleep: str, bark: int, dig: bool):
        self._sleep = sleep
        self._bark = bark
        self._dig = dig


")


(jea-cg--py-variables-split '("ssleep" "ibark" "bdig"))
;; (("sleep" "str") ("bark" "int") ("dig" "bool"))

(jea-cg--py-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool")))
"class Cat:
    \"\"

    def __init__(self, sleep: str, bark: int, dig: bool):
        self._sleep = sleep
        self._bark = bark
        self._dig = dig


")

;; "class Cat:
;;     \"\"
;;
;;     def __init__(self, sleep: str, bark: int, dig: bool):
;;         self._sleep = sleep
;;         self._bark = bark
;;         self._dig = dig
;;
;;
;; "


(jea-cg--py-dict-kv-fmt "foo" "bar" nil nil)
(jea-cg--py-dict-kv-fmt "foo" 14 nil nil)
(jea-cg--py-dict-kv-fmt "bar" 14 nil t)
(jea-cg--py-dict-kv-fmt "foo" 15.4 nil t)


(jea-cg--py-dict "d1" '("one" "2" "three" "four" "five" "6"))

(provide 'jea-code-gen-python-test)

;;; jea-code-gen-python-test.el ends here
