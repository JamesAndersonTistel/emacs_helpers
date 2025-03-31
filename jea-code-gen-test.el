;;; jea-code-gen-test.el --- unit test the code gen

;;; Code:

;; TODO write a string diff thay high lights the difference! char by char.

(defmacro jea-test-text (func expected-text-output)
	"Run the function FUNC and double check the text output is EXPECTED-TEXT-OUTPUT."
	`(let* ((debug nil) ;; turn on prints
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

(defmacro jea-test-list (func expected-list-output)
	"Run the function FUNC and double check the text output is EXPECTED-LIST-OUTPUT."
	`(let* ((debug t) ;; turn on prints
					(output (funcall ,func))
					(success (equal output ,expected-list-output)))
		 (if success
				 (progn
					 (if debug
							 (message "success %s" ,func))
					 t)
			 (progn
				 (if debug
						 (message "failed because %s: '%s' does not equal '%s'." ,func output ,expected-list-output))
				 nil))))

(defmacro jea-test-number (func expected-value)
	"Run the function FUNC and double check the text output is EXPECTED-VALUE."
	`(let* ((debug t) ;; turn on prints
					(output (funcall ,func))
					(success (= output ,expected-value)))
		 (if success
				 (progn
					 (if debug
							 (message "success %s" ,func))
					 t)
			 (progn
				 (if debug
						 (message "failed because %s: '%s' does not equal '%s'." ,func output ,expected-value))
				 nil))))

(provide 'jea-code-gen-test)

;;; jea-code-gen-test.el ends here
