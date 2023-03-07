# Doom modules

These are modules I am using in my doom emacs configuration. Some of them are adapted (straight up copied, in this case) from the amazing work of others.

In the "copy" cases it's because I wanted to doomify the code, rather than using individual prefixes

To make use of this, `git clone <this repo> ~/.doom.d/modules` and then add an `:offby1` section to your `init.el` with these modes:

```emacs-lisp
:offby1
(exp +chatgpt)
```
