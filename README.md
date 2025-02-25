# emacs_helpers
a disorganiszed set of elisp code that I find useful. I have not been putting much into github for the simple fact that I have been working professionally and not had the time.

## jea-password-generate
[jea-password-generate](./README-jea-password-generate.md)

## jea-string-util
[jea-string-util](./README-jea-string-util.md)

## jea-tweet-assist
[tweet-assist](./README-tweet-assist.md)

## jea-code-gen
[jea-code-gen](./README-jea-code-gen.md)

## install

add this to your `.emacs` file:

```
(dolist (fpath '("~/public_github/emacs_helpers/jea-string-util.el"
                 "~/public_github/emacs_helpers/jea-code-gen.el"
                 "~/public_github/emacs_helpers/jea-code-gen-python.el"
                 "~/public_github/emacs_helpers/jea-password-generate.el"
                 "~/public_github/emacs_helpers/jea-tweet-assist.el"))
  (if (file-exists-p fpath)
    (load (expand-file-name fpath))))
```
