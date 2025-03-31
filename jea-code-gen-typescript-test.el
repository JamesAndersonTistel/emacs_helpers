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
				)	
		(and t1 t2 t3 t4 t5 t6)))

(jea-code-gen-test-typescript)

;; right now this is just a copy of the python one.
;; TODO hook up real functions:

;; 
;; jea-cg--ts-variables-split(variables) ;
;; jea-cg--ts-expand-declaration-variables(expanded-vars)
;; jea-cg--ts-expand-ctor-args-variables(expanded-vars)
;; jea-cg--ts-expand-ctor-contents-variables(expanded-vars)
;; jea-cg--ts-class(name variables)
;; jea-cg--ts-func(name &optional exp-args)

(provide 'jea-code-gen-typescript-test)

;;; jea-code-gen-typescript-test.el ends here
