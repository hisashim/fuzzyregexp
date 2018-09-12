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

(defun %fre-replace-substring (str rep start end)
  "Replace substring of STR (from START (inclusive) to END (exclusive))
 with REP"
  (concat (substring str 0 start)
          (cond ((stringp rep) rep)
                ((functionp rep) (funcall rep (substring str start end)))
                (t (error "replacement must be a string or a function")))
          (substring str end)))

(defun %fre-remove-substring (str start end)
  (%fre-replace-substring str "" start end))

(defun %fre-insert-substring (str idx ins)
  (concat (substring str 0 idx)
          ins
          (substring str idx)))

(defun %fre-transpose-substring (str idx)
  (concat (substring str 0 idx)
          (substring str (+ idx 1) (+ idx 2))
          (substring str idx (+ idx 1))
          (substring str (+ idx 2))))

(defun %fre-remove-if-match (str regexps)
  (reduce (lambda (l e)
            (if (string-match e str)
                l
              (append l (list e))))
          regexps
          :initial-value '()))

;; varied patterns

(defun %fre-patterns-one-char-removed (str)
  (cond ((<= (length str) 1) `(""))
        (t (let* ((body-indices (if (<= (length str) 2)
                                    '()
                                  (number-sequence 1 (- (length str) 2))))
                  (fst (concat "[^"
                               (regexp-quote (substring str 0 1))
                               "]"
                               (substring str 1 nil)))
                  (body (map 'list
                             (lambda (i) (%fre-remove-substring str i (+ i 1)))
                             body-indices))
                  (lst (concat (regexp-quote (substring str 0 -1))
                               "[^"
                               (substring str -1 nil)
                               "]")))
             `(,fst ,@body ,lst)))))

(defun %fre-patterns-one-char-inserted (str ins)
  (cond ((< (length str) 2) `(""))
        (t `(,@(map 'list
                    (lambda (idx) (%fre-insert-substring str idx ins))
                    (number-sequence 1 (- (length str) 2)))
             ,(%fre-insert-substring str -1 ins)))))

(defun %fre-patterns-one-char-replaced (str rep)
  (cond ((= (length str) 0) `(""))
        (t (map 'list
                (lambda (i) (%fre-replace-substring str rep i (+ i 1)))
                (number-sequence 0 (- (length str) 1))))))

(defun %fre-patterns-one-char-transposed (str)
  (cond ((<= (length str) 1) `(,str))
        ((= (length str) 2) `(,(%fre-transpose-substring str 0)))
        (t (map 'list
                (lambda (idx) (%fre-transpose-substring str idx))
                (number-sequence 0 (- (length str) 2))))))

(defun %fre-patterns-one-char-modified (str)
  (let* ((patterns
          (append
           (%fre-remove-if-match str (%fre-patterns-one-char-removed str))
           (%fre-patterns-one-char-inserted str ".")
           (%fre-patterns-one-char-replaced
            str (lambda (s) (concat "[^" (regexp-quote s) "]")))
           (%fre-remove-if-match str (%fre-patterns-one-char-transposed str))))
         (patterns-concise (delete-dups (remq nil (remove "" patterns)))))
    patterns-concise))

(defun %fre-one-char-modified (str &optional self-inclusive word-match)
  "A naive substitute for regexp-opt that keeps regexp special
characters as-is (not quoted) internally.
Returns regexp.
If SELF-INCLUSIVE is true, regexp also matches STR itself.
If WORD-MATCH is true, regexp tries to match whole words only (not substrings)."
  (let* ((pats-src (%fre-patterns-one-char-modified str))
         (pats-fin (if self-inclusive (cons str pats-src) pats-src))
         (re-src (mapconcat 'identity pats-fin "\\|"))
         (re-fin (if word-match (apply 'concat `("\\b\\(?:" ,re-src "\\)\\b")) re-src)))
    re-fin))

;; public interface

(defun fuzzy-regexp (str &optional include-self word-match)
  "Generate regexp that matches strings modified from STR by one
character.
If INCLUDE-SELF is true, regexp alsos matches STR itself.
If WORD-MATCH is true, regexp tries to match whole words only (not substrings)."
  (%fre-one-char-modified str include-self word-match))

(provide 'fuzzy-regexp)
