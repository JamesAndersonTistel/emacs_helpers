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
															(jea-cg--ts-variable-split "ncount"))
													 '("count" "number")))
				(t2 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "balive"))
													 '("alive" "boolean")))
				(t3 (jea-test-list '(lambda ()
															(jea-cg--ts-variable-split "sname"))
													 '("name" "string")))
				(t4 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "height" "number" nil nil))
													 " height: number,"))
				(t5 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "name" "string" t nil))
													 "name: string,"))
				(t6 (jea-test-text '(lambda ()
															(jea-cg--ts-variable-fmt "salt" "boolean" nil t))
													 " salt: boolean"))
				(t7 (jea-test-list '(lambda ()
															(jea-cg--ts-variables-split '("ssleep" "nbark" "bdig")))
													 '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
				(t8 (jea-test-text '(lambda ()
															(jea-cg--ts-expand-declaration-variables
															 '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
"  /**
   *
   */
  sleep: string;

  /**
   *
   */
  bark: number;

  /**
   *
   */
  dig: boolean;

"))
				(t9 (jea-test-text '(lambda ()
															(jea-cg--ts-expand-ctor-args-variables '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
													 "sleep: string, bark: number, dig: boolean"))
				(t10 (jea-test-text '(lambda ()
															 (jea-cg--ts-expand-ctor-args-variables '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
														"sleep: string, bark: number, dig: boolean"))
				(t11 (jea-test-text '(lambda ()
															 (jea-cg--ts-expand-ctor-contents-variables '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
"this.sleep = sleep;
    this.bark = bark;
    this.dig = dig;
    "))
				(t12 (jea-test-text '(lambda ()
															 (jea-cg--ts-class "dog" '("ssleep" "nbark" "bdig")))
															 "
/**
 *
 */
class Dog {
  /**
   *
   */
  sleep: string;

  /**
   *
   */
  bark: number;

  /**
   *
   */
  dig: boolean;


  constructor(sleep: string, bark: number, dig: boolean) {
    this.sleep = sleep;
    this.bark = bark;
    this.dig = dig;
    
  }
}
"))
				(t13 (jea-test-text '(lambda ()
															 (jea-cg--ts-func "getPrice" '(("sleep" "string") ("bark" "number") ("dig" "boolean"))))
															 "    getPrice(sleep: string, bark: number, dig: boolean): void {
    }
"))

				)
		(and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)))

(jea-code-gen-test-typescript)

;; right now this is just a copy of the python one.

(provide 'jea-code-gen-typescript-test)

;;; jea-code-gen-typescript-test.el ends here
