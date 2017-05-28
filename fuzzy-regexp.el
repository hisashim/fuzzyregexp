;; -*- encoding: utf-8 -*-
;;
;; fuzzy-regexp: fuzzy-matching regexp generator
;; Copyright (c) 2014 Hisashi Morita
;; License: Public Domain
;;
;; Usage:
;;   (fuzzy-regexp "FOO") ;=> "[^f]oo\\|fo\\|fo[^o]")

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

(defun %fre-one-char-modified (str)
  (mapconcat 'identity (%fre-patterns-one-char-modified str) "\\|"))

(defun %fre-one-char-modified-and-itself (str)
  (mapconcat
   'identity
   (append (%fre-patterns-one-char-modified str) str) "\\|"))

;; public interface

(defun fuzzy-regexp (str)
  (%fre-one-char-modified str))

(provide 'fuzzy-regexp)
