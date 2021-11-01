fuzzy-regexp
========

fuzzy-regexp: Fuzzy-matching regexp generator for Emacs

Copyright (C) 2014 Hisashi Morita

  * Author: Hisashi Morita
  * Keywords: convenience, matching, wp
  * License: Public Domain
  * URL: https://github.com/hisashim/fuzzyregexp

Synopsis
--------

    (fuzzy-regexp STR &optional WORD-MATCH SELF-INCLUSIVE)

Generate regexp to match strings that are modification of `STR` by one character.

  * If `WORD-MATCH` is true, regexp tries to match whole words only (not substrings).
  * If `SELF-INCLUSIVE` is true, regexp alsos matches `STR` itself.

Examples
--------

Plain use without options.

    (fuzzy-regexp "the")
    ;;=> "[^t]he\\|te\\|th[^e]\\|t.he\\|th.e\\|t[^h]e\\|hte\\|teh"
    ;;   Matches "teh", "thi", "this", etc.
    ;;   Does not match "the" itself.

Enable `word-match` option for word-oriented match. (This may not work as
intended for languages such as Chinese or Japanese.)

    (fuzzy-regexp "the" t nil)
    ;;=> "\\b\\(?:[^t]he\\|te\\|th[^e]\\|t.he\\|th.e\\|t[^h]e\\|hte\\|teh\\)\\b"
    ;;   Matches "teh", "thi", etc.
    ;;   Does not match substrings within words such as "thi" in "this".
    ;;   Does not match "the" itself.

Enable `self-inclusive` option to match the search keyword itself.

    (fuzzy-regexp "the" nil t)
    ;;=> "the\\|[^t]he\\|te\\|th[^e]\\|t.he\\|th.e\\|t[^h]e\\|hte\\|teh"
    ;;   Matches "teh", "thi", "this", etc.
    ;;   Matches "the" itself.

If you want case-sensitive/insensitive search, set `case-fold-search`.

    (let ((case-fold-search t))
      (search-forward-regexp (fuzzy-regexp "the")))
    ;;=> Matches "Teh", "teh", "Thi", "thi", "This", "this", etc.
    ;;   Does not match "The" or "the".

Installation
--------

  1. Copy `fuzzy-regexp.el` to somewhere in `load-path`.
  2. Add `(require 'fuzzy-regexp)` to your init file, e.g. `~/.emacs.d/init.el`.
  3. Restart Emacs.

Motivation
--------

I needed a fuzzy matching library which can exclude query keyword itself
from the result, in order to find typographical errors in natural language
text, but could not find one.  So I hacked up this kludge.

References
--------

Fuzzy/ambiguous/approximate matching libraries and applications that I know of:

  * [Helm](https://emacs-helm.github.io/helm/)
  * [fuzzy.el](https://github.com/auto-complete/fuzzy-el)
  * [approx-search](https://github.com/susumuota/approx-search)

You may want to try these first.
