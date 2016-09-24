;; -*- encoding: utf-8 -*-
;;
;; fix-typos: simple typographical error corrector
;; Copyright (c) 2014 Hisashi Morita
;; License: Public Domain
;;
;; Requirements:
;;   fuzzy-regexp.el
;;
;; Usage:
;;   (fix-typos "foo")

(require 'fuzzy-regexp)

(defun fix-typos (str)
  "Fix typographical errors of STR interactively"
  (interactive "MString: ")
  (query-replace-regexp (fuzzy-regexp str) str))
