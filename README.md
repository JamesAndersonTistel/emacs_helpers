# emacs_helpers
a disorganiszed set of elisp code that I find useful. I have not been putting much into github for the simple fact that I have been working professionally and not had the time.

## jea-password-generate

### adds the adibilty to generate password to emacs
very simple code so you are not dependent on some mysterious third
party system. very easy to use org-mode tables for each login system

`jea-make-pwrd-clip-board`
Generate a password and store it on the clip board (aka `kill-ring`).

`jea-make-pwrd-insert-buffer`
Generate a password and insert it into the current buffer.

## jea-string-util

`jea-find-string-all-indexes`

examples:
```
(jea-find-string-all-indexes "The quick fox jumped quickly." "\\(qu\\)\\(ick\\)")
((4 6) (6 9) (21 23) (23 26))

(jea-find-string-all-indexes "The quick fox jumped quickly." "\\(qu\\)")
((4 6) (21 23))

(jea-find-string-all-indexes "The quick fox jumped quickly." "fox")
nil
```

`jea-find-string-all`

examples:

```
(jea-find-string-all "The quick fox jumped quickly." "\\(qu\\)\\(ick\\)")
("qu" "ick" "qu" "ick")

(jea-find-string-all "The quick fox jumped quickly." "\\(qu\\)")
("qu" "qu")

(jea-find-string-all "The quick fox jumped quickly.  The fat fox waddled a bit." "\\(f[a-zA-Z]+x\\)")
("fox" "fox")

(jea-find-string-all "The quick fox jumped quickly.  The fat fox waddled a bit." "\\([^.]+\\.\\)")
("The quick fox jumped quickly." "  The fat fox waddled a bit.")

(jea-find-string-all "The quick fox jumped quickly." "fox")
nil
```

`jea-string-ltrim`
example:
```
"  	   	The quick fox jumped quickly."
"The quick fox jumped quickly."
```

`jea-string-rtrim`
example:
```
"The quick fox jumped quickly.  	  	"
"The quick fox jumped quickly."
```

`jea-string-trim`
example: 
```
"	  	  	The quick fox jumped quickly.  	  	"
"The quick fox jumped quickly."
```

`jea-string-util-camel-case-to-snake`

"HelloBraveNewWorld"

becomes:

"hello_brave_new_world"

`jea-string-upcase-snake-case-to-camel` 

"james_anderson__was_here"

becomes:

"JamesAndersonWasHere"

## jea-tweet-assist

This is code to grab from the emacs region and break it into social media size chucks. then the user can cut and paste into browser. In the future, may connect it to APIs.

`jea-tweet-split-long-buffer`

input:

Tempor purus nisl nisl feugiat hendrerit eros convallis sit. Ac luctus eu penatibus ante porta rutrum diam fusce conubia. Conubia blandit convallis elementum rutrum ultricies vehicula. Fames congue vitae neque molestie vitae montes porttitor. Mauris facilisi urna quisque nostra dui; sociosqu duis ridiculus. Ornare felis ullamcorper praesent eu elementum phasellus suscipit dignissim sapien. Phasellus elit libero elementum venenatis blandit pellentesque amet. Congue habitant primis tempus leo congue fusce sagittis himenaeos feugiat.


output:

Tempor purus nisl nisl feugiat hendrerit eros convallis sit. Ac luctus eu penatibus ante porta rutrum diam fusce conubia. Conubia blandit convallis elementum rutrum ultricies vehicula. Fames congue vitae neque molestie vitae montes porttitor. 1/3

Mauris facilisi urna quisque nostra dui; sociosqu duis ridiculus. Ornare felis ullamcorper praesent eu elementum phasellus suscipit dignissim sapien. Phasellus elit libero elementum venenatis blandit pellentesque amet. 2/3

Congue habitant primis tempus leo congue fusce sagittis himenaeos feugiat. 3/3

