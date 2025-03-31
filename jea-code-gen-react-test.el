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
															(jea-cg--react-class "Game"
																									 '(("sleep" "str") ("bark" "int") ("dig" "bool"))))
"// Copyright © 2025 James Anderson
//
// Author: James Anderson <james@tisteltech.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files \\(the \"Software\"\\), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so.
//
// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import { useState } from \"react\";

export default function Game() {
    const [sleep, setSleep] = useState(\"\");
    const [bark, setBark] = useState(0);
    const [dig, setDig] = useState(false);

    return (<></>);
}")))
		(and t1)))

(jea-code-gen-test-react)

;; right now this is just a copy of the python one.

(provide 'jea-code-gen-react-test)

;;; jea-code-gen-react-test.el ends here
