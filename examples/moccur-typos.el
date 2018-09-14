;; -*- encoding: utf-8 -*-
;;
;; moccur-typos: simple typographical error checker using color-moccur
;; Copyright (c) 2014 Hisashi Morita
;; License: Public Domain
;;
;; Requirements:
;;   color-moccur.el, fuzzy-regexp.el
;;
;; Usage:
;;   (moccur-typos "foo") ;=> "xoo", "fxo", "fox"... , but not "foo"

(require 'color-moccur)
(require 'fuzzy-regexp)

(defun moccur-typos-optimistic (str)
  "Show possible typographical errors of STR, off by a single character.
Show fewer candidates, avoiding false positives.
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
Show many candidates, allowing false positives.
  - Case insensitive
  - Match substrings
  - Include the search string itself"
  (interactive "MString: ")
  (let ((case-fold-search t))
    (moccur (fuzzy-regexp str nil t) t)))

(defalias 'moccur-typos 'moccur-typos-moderate)
