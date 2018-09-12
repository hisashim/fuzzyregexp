;; -*- encoding: utf-8 -*-
;;
;; tests for fuzzy-regexp
;; Copyright (c) 2014 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'fuzzy-regexp)

(ert-deftest test-%fre-replace-substring ()
  (should (equal
           '("aXcdef"
             "abXf"
             "abCDef"
             "ab<C>def")
           `(,(%fre-replace-substring "abcdef" "X" 1 2)
             ,(%fre-replace-substring "abcdef" "X" 2 5)
             ,(%fre-replace-substring "abcdef" (lambda (s) (upcase s)) 2 4)
             ,(%fre-replace-substring "abcdef"
                                  (lambda (s) (concat "<" (upcase s) ">"))
                                  2 3)))))

(ert-deftest test-%fre-remove-substring ()
  (should (equal
           '("ac"
             "abf")
           `(,(%fre-remove-substring "abc" 1 2)
             ,(%fre-remove-substring "abcdef" 2 5)))))

(ert-deftest test-%fre-insert-substring ()
  (should (equal
           '("XYabc"
             "aXYbc")
           `(,(%fre-insert-substring "abc" 0 "XY")
             ,(%fre-insert-substring "abc" 1 "XY")))))

(ert-deftest test-%fre-transpose-substring ()
  (should (equal
           '("ba"
             "bac"
             "acb")
           `(,(%fre-transpose-substring "ab" 0)
             ,(%fre-transpose-substring "abc" 0)
             ,(%fre-transpose-substring "abc" 1)))))

(ert-deftest test-%fre-patterns-one-char-removed ()
  (should (equal
           '(("")
             ("")
             ("[^a]b" "a[^b]")
             ("[^a]bc" "ac" "ab[^c]")
             ("[^a]bcd" "acd" "abd" "abc[^d]"))
           `(,(%fre-patterns-one-char-removed "")
             ,(%fre-patterns-one-char-removed "a")
             ,(%fre-patterns-one-char-removed "ab")
             ,(%fre-patterns-one-char-removed "abc")
             ,(%fre-patterns-one-char-removed "abcd")))))

(ert-deftest test-%fre-patterns-one-char-inserted ()
  (should (equal
           '(("")
             ("")
             ("axb")
             ("axbc" "abxc"))
           `(,(%fre-patterns-one-char-inserted "" "x")
             ,(%fre-patterns-one-char-inserted "a" "x")
             ,(%fre-patterns-one-char-inserted "ab" "x")
             ,(%fre-patterns-one-char-inserted "abc" "x")))))

(ert-deftest test-%fre-patterns-one-char-replaced ()
  (should (equal
           '(("")
             ("x")
             ("xb" "ax")
             ("xbc" "axc" "abx"))
           `(,(%fre-patterns-one-char-replaced "" "x")
             ,(%fre-patterns-one-char-replaced "a" "x")
             ,(%fre-patterns-one-char-replaced "ab" "x")
             ,(%fre-patterns-one-char-replaced "abc" "x")))))

(ert-deftest test-%fre-patterns-one-char-transposed ()
  (should (equal
           '(("")
             ("a")
             ("ba")
             ("bac" "acb")
             ("bacd" "acbd" "abdc"))
           `(,(%fre-patterns-one-char-transposed "")
             ,(%fre-patterns-one-char-transposed "a")
             ,(%fre-patterns-one-char-transposed "ab")
             ,(%fre-patterns-one-char-transposed "abc")
             ,(%fre-patterns-one-char-transposed "abcd")))))

(ert-deftest test-%fre-patterns-one-char-modified ()
  (should (equal
           '(()
             ("[^a]")
             ("[^a]b" "a[^b]" "a.b" "ba")
             ("[^a]bc" "ac" "ab[^c]"
              "a.bc" "ab.c" "a[^b]c" "bac" "acb")
             ("[^a]a" "a[^a]" "a.a")
             ("[^a]bb" "ab[^b]"
              "a.bb" "ab.b" "a[^b]b" "bab"))
           `(,(%fre-patterns-one-char-modified "")
             ,(%fre-patterns-one-char-modified "a")
             ,(%fre-patterns-one-char-modified "ab")
             ,(%fre-patterns-one-char-modified "abc")
             ,(%fre-patterns-one-char-modified "aa")
             ,(%fre-patterns-one-char-modified "abb")))))

(ert-deftest test-%fre-one-char-modified ()
  (should (equal
           '(""
             "[^a]"
             "[^a]b\\|a[^b]\\|a.b\\|ba"
             "[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "[^a]a\\|a[^a]\\|a.a"
             "[^a]bb\\|ab[^b]\\|a.bb\\|ab.b\\|a[^b]b\\|bab")
           `(,(%fre-one-char-modified "")
             ,(%fre-one-char-modified "a")
             ,(%fre-one-char-modified "ab")
             ,(%fre-one-char-modified "abc")
             ,(%fre-one-char-modified "aa")
             ,(%fre-one-char-modified "abb")))))

(ert-deftest test-%fre-one-char-modified-self-inclusive ()
  (should (equal
           '("[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "abc\\|[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb")
           `(,(%fre-one-char-modified "abc" nil)
             ,(%fre-one-char-modified "abc" t)))))

(ert-deftest test-%fre-one-char-modified-word-match ()
  (should (equal
           '("[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "\\b\\(?:[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb\\)\\b")
           `(,(%fre-one-char-modified "abc" nil nil)
             ,(%fre-one-char-modified "abc" nil t)))))

(ert-deftest test-fuzzy-regexp ()
  (should (equal
           '(0
             nil)
           `(,(string-match (fuzzy-regexp "abc") "abX")
             ,(string-match (fuzzy-regexp "abc") "abc")))))

(ert-deftest test-fuzzy-regexp-self-inclusive ()
  (should (equal
           '(0
             nil
             0
             0)
           `(,(string-match (fuzzy-regexp "abc" nil) "abX")
             ,(string-match (fuzzy-regexp "abc" nil) "abc")
             ,(string-match (fuzzy-regexp "abc" t) "abX")
             ,(string-match (fuzzy-regexp "abc" t) "abc")))))

(ert-deftest test-fuzzy-regexp-word-match ()
  (should (equal
           '(1
             1
             1
             nil)
           `(,(string-match (fuzzy-regexp "abc" nil nil) " abX ")
             ,(string-match (fuzzy-regexp "abc" nil nil) "zabXz")
             ,(string-match (fuzzy-regexp "abc" nil t) " abX ")
             ,(string-match (fuzzy-regexp "abc" nil t) "zabXz")))))

;; (ert-run-tests-batch-and-exit)
