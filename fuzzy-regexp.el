;; -*- encoding: utf-8 -*-
;;
;; fuzzy-regexp: fuzzy-matching regexp generator
;; Copyright (c) 2014 Hisashi Morita
;; License: Public Domain
;;
;; Usage:
;;   (fuzzy-regexp "foo") ;=> "[^f]oo\\|fo[^o]\\|f.oo\\|fo.o\\|f[^o]o\\|ofo"
;;                        ;   which matches goo, fog, fgoo, fogo, ofo, etc.,
;;                        ;   but does not match foo itself.
;;
;; Motivation:
;;   I needed a fuzzy matching library for finding typos which can
;;   exclude query keyword itself from the result, but could not find
;;   one. So I hacked up a kludge.

(eval-when-compile (require 'cl))

;; string utilities

(defun fuzzy-regexp--replace-substring (str rep start end)
  "Replace substring of STR (from START (inclusive) to END (exclusive))
 with REP"
  (concat (substring str 0 start)
          (cond ((stringp rep) rep)
                ((functionp rep) (funcall rep (substring str start end)))
                (t (error "replacement must be a string or a function")))
          (substring str end)))

(defun fuzzy-regexp--remove-substring (str start end)
  (fuzzy-regexp--replace-substring str "" start end))

(defun fuzzy-regexp--insert-substring (str idx ins)
  (concat (substring str 0 idx)
          ins
          (substring str idx)))

(defun fuzzy-regexp--transpose-substring (str idx)
  (concat (substring str 0 idx)
          (substring str (+ idx 1) (+ idx 2))
          (substring str idx (+ idx 1))
          (substring str (+ idx 2))))

(defun fuzzy-regexp--remove-if-match (str regexps)
  (reduce (lambda (l e)
            (if (string-match e str)
                l
              (append l (list e))))
          regexps
          :initial-value '()))

;; varied patterns

(defun fuzzy-regexp--patterns-one-char-removed (str)
  (cond ((<= (length str) 1) `(""))
        (t (let* ((body-indices (if (<= (length str) 2)
                                    '()
                                  (number-sequence 1 (- (length str) 2))))
                  (fst (concat "[^"
                               (regexp-quote (substring str 0 1))
                               "]"
                               (substring str 1 nil)))
                  (body (map 'list
                             (lambda (i)
                               (fuzzy-regexp--remove-substring str i (+ i 1)))
                             body-indices))
                  (lst (concat (regexp-quote (substring str 0 -1))
                               "[^"
                               (substring str -1 nil)
                               "]")))
             `(,fst ,@body ,lst)))))

(defun fuzzy-regexp--patterns-one-char-inserted (str ins)
  (cond ((< (length str) 2) `(""))
        (t `(,@(map 'list
                    (lambda (idx) (fuzzy-regexp--insert-substring str idx ins))
                    (number-sequence 1 (- (length str) 2)))
             ,(fuzzy-regexp--insert-substring str -1 ins)))))

(defun fuzzy-regexp--patterns-one-char-replaced (str rep)
  (cond ((= (length str) 0) `(""))
        (t (map 'list
                (lambda (i) (fuzzy-regexp--replace-substring str rep i (+ i 1)))
                (number-sequence 0 (- (length str) 1))))))

(defun fuzzy-regexp--patterns-one-char-transposed (str)
  (cond ((<= (length str) 1) `(,str))
        ((= (length str) 2) `(,(fuzzy-regexp--transpose-substring str 0)))
        (t (map 'list
                (lambda (idx) (fuzzy-regexp--transpose-substring str idx))
                (number-sequence 0 (- (length str) 2))))))

(defun fuzzy-regexp--patterns-one-char-modified (str)
  (let* ((patterns
          (append
           (fuzzy-regexp--remove-if-match
            str (fuzzy-regexp--patterns-one-char-removed str))
           (fuzzy-regexp--patterns-one-char-inserted str ".")
           (fuzzy-regexp--patterns-one-char-replaced
            str (lambda (s) (concat "[^" (regexp-quote s) "]")))
           (fuzzy-regexp--remove-if-match
            str (fuzzy-regexp--patterns-one-char-transposed str))))
         (patterns-concise (delete-dups (remq nil (remove "" patterns)))))
    patterns-concise))

(defun fuzzy-regexp--one-char-modified (str &optional word-match self-inclusive)
  "A naive substitute for regexp-opt that keeps regexp special
characters as-is (not quoted) internally.
Returns regexp.
If WORD-MATCH is true, regexp tries to match whole words only (not substrings).
If SELF-INCLUSIVE is true, regexp also matches STR itself."
  (let* ((pats-src (fuzzy-regexp--patterns-one-char-modified str))
         (pats-fin (if self-inclusive (cons str pats-src) pats-src))
         (re-src (mapconcat 'identity pats-fin "\\|"))
         (re-fin (if word-match (apply 'concat `("\\b\\(?:" ,re-src "\\)\\b")) re-src)))
    re-fin))

;; public interface

(defun fuzzy-regexp (str &optional word-match include-self)
  "Generate regexp that matches strings modified from STR by one
character.
If WORD-MATCH is true, regexp tries to match whole words only (not substrings).
If INCLUDE-SELF is true, regexp alsos matches STR itself."
  (fuzzy-regexp--one-char-modified str word-match include-self))

(provide 'fuzzy-regexp)
