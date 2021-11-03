;;; moccur-typos.el --- Simple typographical error checker
;;; -*- encoding: utf-8 -*-

;; Copyright (C) 2014 Hisashi Morita

;; Author: Hisashi Morita
;; Keywords: convenience, matching, wp
;; Package-Requires: (fuzzy-regexp, color-moccur)
;; License: Public Domain

;;; Commentary:

;; Setup:
;;
;;   1. Install color-moccur using package manager.  (See https://melpa.org .)
;;   2. Copy fuzzy-regexp.el and moccur-typos.el (this file) to somewhere in
;;      `load-path'.
;;   3. Add (require 'moccur-typos) to your init file, e.g. ~/.emacs.d/init.el.
;;   4. Restart Emacs.
;;
;; Usage:
;;
;;   1. M-x moccur-typos
;;      foo
;;      ;;=> Matches "xoo", "fxo", "fox"... , but not "foo"
;;   2. Edit as you like.  (See color-moccur.el for its usage.)

;;; Code:

(require 'color-moccur)
(require 'fuzzy-regexp)

(defun moccur-typos-optimistic (str)
  "Show possible typographical errors of STR, off by a single character.
Show fewer candidates to reduce false positives.
  - Case sensitive
  - Match whole words only"
  (interactive "MString: ")
  (let ((case-fold-search nil))
    (moccur (fuzzy-regexp str t nil) t)))

(defun moccur-typos-moderate (str)
  "Show possible typographical errors of STR, off by a single character.
Show moderate number of candidates.
  - Case insensitive
  - Match substrings"
  (interactive "MString: ")
  (let ((case-fold-search t))
    (moccur (fuzzy-regexp str nil nil) t)))

(defun moccur-typos-pessimistic (str)
  "Show possible typographical errors of STR, off by a single character.
Show many candidates allowing false positives.
  - Case insensitive
  - Match substrings
  - Include the search string itself"
  (interactive "MString: ")
  (let ((case-fold-search t))
    (moccur (fuzzy-regexp str nil t) t)))

(defalias 'moccur-typos 'moccur-typos-moderate)

(provide 'moccur-typos)

;;; moccur-typos.el ends here
