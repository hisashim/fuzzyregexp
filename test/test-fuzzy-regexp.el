;; -*- encoding: utf-8 -*-
;;
;; Developer tests for fuzzy-regexp
;; Copyright (C) 2014 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'fuzzy-regexp)

(ert-deftest test-fuzzy-regexp--replace-substring ()
  (should (equal
           '("aXcdef"
             "abXf"
             "abCDef"
             "ab<C>def")
           `(,(fuzzy-regexp--replace-substring "abcdef" "X" 1 2)
             ,(fuzzy-regexp--replace-substring "abcdef" "X" 2 5)
             ,(fuzzy-regexp--replace-substring "abcdef" (lambda (s) (upcase s)) 2 4)
             ,(fuzzy-regexp--replace-substring "abcdef"
                                  (lambda (s) (concat "<" (upcase s) ">"))
                                  2 3)))))

(ert-deftest test-fuzzy-regexp--remove-substring ()
  (should (equal
           '("ac"
             "abf")
           `(,(fuzzy-regexp--remove-substring "abc" 1 2)
             ,(fuzzy-regexp--remove-substring "abcdef" 2 5)))))

(ert-deftest test-fuzzy-regexp--insert-substring ()
  (should (equal
           '("XYabc"
             "aXYbc")
           `(,(fuzzy-regexp--insert-substring "abc" 0 "XY")
             ,(fuzzy-regexp--insert-substring "abc" 1 "XY")))))

(ert-deftest test-fuzzy-regexp--transpose-substring ()
  (should (equal
           '("ba"
             "bac"
             "acb"
             "bacd"
             "acbd")
           `(,(fuzzy-regexp--transpose-substring "ab" 0)
             ,(fuzzy-regexp--transpose-substring "abc" 0)
             ,(fuzzy-regexp--transpose-substring "abc" 1)
             ,(fuzzy-regexp--transpose-substring "abcd" 0)
             ,(fuzzy-regexp--transpose-substring "abcd" 1)
             ))))

(ert-deftest test-fuzzy-regexp--patterns-one-char-removed ()
  (should (equal
           '(("")
             ("")
             ("[^a]b" "a[^b]")
             ("[^a]bc" "ac" "ab[^c]")
             ("[^a]bcd" "acd" "abd" "abc[^d]"))
           `(,(fuzzy-regexp--patterns-one-char-removed "")
             ,(fuzzy-regexp--patterns-one-char-removed "a")
             ,(fuzzy-regexp--patterns-one-char-removed "ab")
             ,(fuzzy-regexp--patterns-one-char-removed "abc")
             ,(fuzzy-regexp--patterns-one-char-removed "abcd")))))

(ert-deftest test-fuzzy-regexp--patterns-one-char-inserted ()
  (should (equal
           '(("")
             ("")
             ("axb")
             ("axbc" "abxc"))
           `(,(fuzzy-regexp--patterns-one-char-inserted "" "x")
             ,(fuzzy-regexp--patterns-one-char-inserted "a" "x")
             ,(fuzzy-regexp--patterns-one-char-inserted "ab" "x")
             ,(fuzzy-regexp--patterns-one-char-inserted "abc" "x")))))

(ert-deftest test-fuzzy-regexp--patterns-one-char-replaced ()
  (should (equal
           '(("")
             ("x")
             ("xb" "ax")
             ("xbc" "axc" "abx"))
           `(,(fuzzy-regexp--patterns-one-char-replaced "" "x")
             ,(fuzzy-regexp--patterns-one-char-replaced "a" "x")
             ,(fuzzy-regexp--patterns-one-char-replaced "ab" "x")
             ,(fuzzy-regexp--patterns-one-char-replaced "abc" "x")))))

(ert-deftest test-fuzzy-regexp--patterns-one-char-transposed ()
  (should (equal
           '(("")
             ("a")
             ("ba")
             ("bac" "acb")
             ("bacd" "acbd" "abdc"))
           `(,(fuzzy-regexp--patterns-one-char-transposed "")
             ,(fuzzy-regexp--patterns-one-char-transposed "a")
             ,(fuzzy-regexp--patterns-one-char-transposed "ab")
             ,(fuzzy-regexp--patterns-one-char-transposed "abc")
             ,(fuzzy-regexp--patterns-one-char-transposed "abcd")))))

(ert-deftest test-fuzzy-regexp--patterns-one-char-modified ()
  (should (equal
           '(()
             ("[^a]")
             ("[^a]b" "a[^b]" "a.b" "ba")
             ("[^a]bc" "ac" "ab[^c]"
              "a.bc" "ab.c" "a[^b]c" "bac" "acb")
             ("[^a]a" "a[^a]" "a.a")
             ("[^a]bb" "ab[^b]"
              "a.bb" "ab.b" "a[^b]b" "bab"))
           `(,(fuzzy-regexp--patterns-one-char-modified "")
             ,(fuzzy-regexp--patterns-one-char-modified "a")
             ,(fuzzy-regexp--patterns-one-char-modified "ab")
             ,(fuzzy-regexp--patterns-one-char-modified "abc")
             ,(fuzzy-regexp--patterns-one-char-modified "aa")
             ,(fuzzy-regexp--patterns-one-char-modified "abb")))))

(ert-deftest test-fuzzy-regexp--one-char-modified ()
  (should (equal
           '(""
             "[^a]"
             "[^a]b\\|a[^b]\\|a.b\\|ba"
             "[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "[^a]a\\|a[^a]\\|a.a"
             "[^a]bb\\|ab[^b]\\|a.bb\\|ab.b\\|a[^b]b\\|bab")
           `(,(fuzzy-regexp--one-char-modified "")
             ,(fuzzy-regexp--one-char-modified "a")
             ,(fuzzy-regexp--one-char-modified "ab")
             ,(fuzzy-regexp--one-char-modified "abc")
             ,(fuzzy-regexp--one-char-modified "aa")
             ,(fuzzy-regexp--one-char-modified "abb")))))

(ert-deftest test-fuzzy-regexp--one-char-modified-word-match ()
  (should (equal
           '("[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "\\b\\(?:[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb\\)\\b")
           `(,(fuzzy-regexp--one-char-modified "abc" nil)
             ,(fuzzy-regexp--one-char-modified "abc" t)))))

(ert-deftest test-fuzzy-regexp--one-char-modified-self-inclusive ()
  (should (equal
           '("[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb"
             "abc\\|[^a]bc\\|ac\\|ab[^c]\\|a.bc\\|ab.c\\|a[^b]c\\|bac\\|acb")
           `(,(fuzzy-regexp--one-char-modified "abc" nil nil)
             ,(fuzzy-regexp--one-char-modified "abc" nil t)))))

(ert-deftest test-fuzzy-regexp ()
  (should (equal
           '(0
             nil)
           `(,(string-match (fuzzy-regexp "abc") "abX")
             ,(string-match (fuzzy-regexp "abc") "abc")))))

(ert-deftest test-fuzzy-regexp-word-match ()
  (should (equal
           '(1
             1
             1
             nil)
           `(,(string-match (fuzzy-regexp "abc" nil) " abX ")
             ,(string-match (fuzzy-regexp "abc" nil) "zabXz")
             ,(string-match (fuzzy-regexp "abc" t) " abX ")
             ,(string-match (fuzzy-regexp "abc" t) "zabXz")))))

(ert-deftest test-fuzzy-regexp-self-inclusive ()
  (should (equal
           '(0
             nil
             0
             0)
           `(,(string-match (fuzzy-regexp "abc" nil nil) "abX")
             ,(string-match (fuzzy-regexp "abc" nil nil) "abc")
             ,(string-match (fuzzy-regexp "abc" nil t) "abX")
             ,(string-match (fuzzy-regexp "abc" nil t) "abc")))))

;;; test-fuzzy-regexp.el ends here
