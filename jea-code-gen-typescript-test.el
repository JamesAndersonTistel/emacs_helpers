;;; jea-code-gen-typescript-test.el --- test the typescript code gen

;;; Code:

(require 'jea-code-gen-typescript)
(require 'jea-code-gen-test)

;; need to setup a proper unit test system soon
;; just moving the test code out
;; START here need to be more systematic and bring over each func.
;; pass in debug as a arg with default off

(defun jea-code-gen-test-typescript()
	"Test all typescript functions."
	(let ((t1 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "icount"))
													 '("count" "int")))
				(t2 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "fweight"))
													 '("weight" "float")))
				(t3 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "balive"))
													 '("alive" "bool")))
				(t4 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "sname"))
													 '("name" "str")))
				(t5 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "ucount"))
													 '("count" "union")))
				(t6 (jea-test-list '(lambda ()
															(jea-cg--ts-variables-split
															 '("ssleep" "ibark" "bdig" "sswim")))
													 '(("sleep" "str") ("bark" "int") ("dig" "bool") ("swim" "str"))))
				(t7 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "height" "float" nil nil))
													 " height: float,")) ;; note leading space
				(t8 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "height" "float" t nil))
													 "height: float,"))
				(t9 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "height" "float" nil t))
													 " height: float")) ;; note the trailing comma is gone
				(t10 (jea-test-text '(lambda () (jea-cg--ts-ctor "cat" '(("sleep" "str") ("bark" "int") ("dig" "bool"))))
"class Cat:
    \"\"

    def __init__(self, sleep: str, bark: int, dig: bool):
        self._sleep = sleep
        self._bark = bark
        self._dig = dig


"))
				(t11 (jea-test-text '(lambda ()
															 (jea-cg--ts-func "hello"
																								'(("sleep" "str") ("bark" "int") ("dig" "bool") ("swim" "str"))))
"    def hello(self, sleep: str, bark: int, dig: bool, swim: str):
        \"\"
        result = None
        return result

"))
				(t12 (jea-test-text '(lambda () (jea-cg--ts-func "hello"))
"    def hello(self):
        \"\"
        result = None
        return result

"))
				(t13 (jea-test-text '(lambda () (jea-cg--ts-add--compare "x" 2)) "x == 2"))
				(t14 (jea-test-text '(lambda () (jea-cg--ts-add--compare "y" "bar")) "y == 'bar'"))
				(t15 (jea-test-text '(lambda () (jea-cg--ts-switch "x" '(1 2 3)))
"    if x == 1:
        pass
    elif x == 2:
        pass
    elif x == 3:
        pass
    else:
        pass
"))
				(t16 (jea-test-text '(lambda () (jea-cg--ts-switch "x" '("dog" "cat" 3)))
"    if x == 'dog':
        pass
    elif x == 'cat':
        pass
    elif x == 3:
        pass
    else:
        pass
"))
				(t17 (jea-test-text '(lambda ()
															 (jea-cg--ts-dict "d1"
																								'("a" "1" "bee" "42.33" "see" "far")))
"d1 = {
    'a': 1,
    'bee': 42.33,
    'see': 'far'
}
")))
	
		(and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17)))

(jea-code-gen-test-typescript)

(provide 'jea-code-gen-typescript-test)

;;; jea-code-gen-typescript-test.el ends here

