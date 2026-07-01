;;; test-pewcfg-use-package.el --- unit tests for pewcfg-use-package -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:

;; Unit tests for `pewcfg-use-package'.
;;
;; Conventions:
;; - Macros are checked with `should-expand-to'.
;; - Everything else uses plain `should'.

;;; Code:

(require 'ert)
(require 'prelude)
(require 'pewcfg-use-package)

;;; Test dummies
(add-to-list 'pewcfg-keywords :unittest)

(defun pewcfg--normalize-:unittest (forms)
  forms)

(defun pewcfg--generate-:unittest (&rest args)
  (list args))

;;; Helper functions
(ert-deftest pewcfg-use-package-test-translate-keyword ()
  (should (equal nil (pewcfg-use-package-translate-pewcfg-keyword :init)))
  (should (equal nil (pewcfg-use-package-translate-pewcfg-keyword :config/)))
  (should (equal '(:config . :something)
                 (pewcfg-use-package-translate-pewcfg-keyword :config/something)))
  (should (equal '(:init . :something)
                 (pewcfg-use-package-translate-pewcfg-keyword :init/something))))

;;; pewcfg-use-package macro expansion
(ert-deftest pewcfg-use-package-test-expand ()
  (should-expand-to (pewcfg-use-package emacs
                      :custom
                      (aaa val)
                      :config
                      (bbb)
                      (ccc)
                      :config/unittest
                      (ddd 123)
                      (eee 321)
                      :init/unittest
                      (foo 666)
                      (bar 888))
                    `(use-package emacs
                       :custom
                       (aaa val)
                       :config
                       (bbb)
                       (ccc)
                       :config
                       (progn (ddd 123)
                              (eee 321))
                       :init
                       (progn (foo 666)
                              (bar 888)))))

(provide 'test-pewcfg-use-package)
;;; test-pewcfg-use-package.el ends here
