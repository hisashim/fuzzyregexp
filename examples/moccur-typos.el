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

(defun moccur-typos (str)
  "Show possible typographical errors of STR by single character"
  (interactive "MString: ")
  (moccur (fuzzy-regexp str) t))
