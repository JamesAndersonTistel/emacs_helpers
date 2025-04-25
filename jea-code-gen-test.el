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
				 (message "failed because %s: '%s' does not equal '%s'." ,func output ,expected-text-output)
				 nil))))

(defmacro jea-test-list (func expected-list-output)
	"Run the function FUNC and double check the text output is EXPECTED-LIST-OUTPUT."
	`(let* ((debug nil) ;; turn on prints
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
	`(let* ((debug nil) ;; turn on prints
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

(defun jea-test-get-long-random-text1 ()
		"Long string test."
	(with-suppressed-warnings ()
		"Lorem ipsum odor amet, consectetuer adipiscing elit. Dolor nec magnis aliquet risus dapibus mi tempus lorem. Maximus ultrices faucibus varius lectus fames dictum sodales per. Eget nisi ridiculus eu fusce ornare nisl. Dui pellentesque aptent efficitur vulputate feugiat luctus enim. Inceptos dictum ultricies, mollis venenatis finibus donec habitasse. Vehicula malesuada tellus nec dis phasellus; sit varius felis diam. Lobortis rutrum faucibus lacus dapibus cubilia volutpat faucibus congue.

Sed orci fermentum nisl ipsum leo blandit. Nascetur eros hac bibendum orci laoreet etiam laoreet est tristique. Litora magnis dictum mi penatibus venenatis tincidunt eu etiam mattis. Eu potenti iaculis pulvinar venenatis tristique gravida placerat ad aenean. Dolor duis cursus malesuada praesent varius dis. Rutrum habitasse ligula curae vitae proin feugiat auctor mauris dolor. Malesuada rutrum morbi netus cursus natoque a cubilia condimentum non. Platea ante ipsum sociosqu curabitur pretium.





Donec ut habitasse fermentum dolor, conubia est. Habitasse urna metus arcu torquent consectetur leo. Magna vestibulum tellus montes auctor luctus sem condimentum at. Gravida dis nunc consequat mattis turpis vel condimentum dignissim. Magnis curae fames mi, dis quis cursus bibendum. Gravida semper ultricies platea nulla massa aptent. Netus non et; nam auctor mollis laoreet. Turpis eros enim; sollicitudin justo vivamus hac vivamus.

Phasellus nec elit, nullam dolor porta netus vulputate laoreet. Eu congue ultricies risus ultricies aenean scelerisque natoque. Natoque dictum id pharetra orci dui. Efficitur       faucibus ridiculus phasellus proin tortor litora. Nisi gravida euismod fringilla netus pellentesque vestibulum. Semper faucibus lobortis lacinia curabitur posuere habitasse dui dictum.



Tempor purus nisl nisl feugiat hendrerit eros convallis sit. Ac luctus eu penatibus ante porta rutrum diam fusce conubia. Conubia blandit convallis elementum rutrum ultricies vehicula. Fames congue vitae neque molestie vitae montes porttitor. Mauris facilisi urna quisque nostra dui; sociosqu duis ridiculus. Ornare felis ullamcorper praesent eu elementum phasellus suscipit dignissim sapien. Phasellus elit libero elementum venenatis blandit pellentesque amet. Congue habitant primis tempus leo congue fusce sagittis himenaeos feugiat."))

(defun jea-test-get-short-random-text1()
	"Lorem ipsum odor amet, consectetuer adipiscing elit.")

(provide 'jea-code-gen-test)

;;; jea-code-gen-test.el ends here
