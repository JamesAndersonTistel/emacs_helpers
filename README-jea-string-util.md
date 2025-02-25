# jea-string-util

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

