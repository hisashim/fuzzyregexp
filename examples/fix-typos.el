;;; fix-typos.el --- Simple typographical error corrector
;;; -*- encoding: utf-8 -*-

;; Copyright (C) 2014 Hisashi Morita

;; Author: Hisashi Morita
;; Keywords: convenience, matching, wp
;; Package-Requires: (fuzzy-regexp)
;; License: Public Domain

;;; Commentary:

;; Setup:
;;
;;   1. Copy fuzzy-regexp.el and fix-typos.el (this file) to somewhere in
;;      `load-path'.
;;   2. Add (require 'fix-typos) to your init file, e.g. ~/.emacs.d/init.el.
;;   3. Restart Emacs.
;;
;; Usage:
;;
;;   1. Switch to the buffer to edit.
;;   2. M-x fix-typos

;;; Code:

(require 'fuzzy-regexp)

(defun fix-typos (str)
  "Fix typographical errors of STR interactively."
  (interactive "MString: ")
  (query-replace-regexp (fuzzy-regexp str) str))

;;; fix-typos.el ends here
