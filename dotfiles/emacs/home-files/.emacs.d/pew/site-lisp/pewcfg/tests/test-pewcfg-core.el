;;; test-pewcfg-core.el --- unit tests for pewcfg-core -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:

;; Unit tests for `pewcfg-core'.
;;
;; Conventions:
;; - `pewcfg--generate-*' and `pewcfg--normalize-*' are checked with
;;   `should-match' against a `pcase' pattern of their output.
;; - Macros are checked with `should-expand-to'.
;; - Everything else uses plain `should'/`should-error'.

;;; Code:

(require 'ert)
(require 'prelude)
(require 'pewcfg-core)

;;; Test dummies
(add-to-list 'pewcfg-keywords :unittest)

(defun pewcfg--normalize-:unittest (forms)
  forms)

(defun pewcfg--generate-:unittest (&rest args)
  (list args))

(defcustom unittest-var nil "Dummy variable for testing."
  :type 'symbol
  :group 'pewcfg)

;;; Utility functions
(ert-deftest pewcfg-core-test-normalize-identity ()
  (should (equal 'foo (pewcfg-normalize-identity 'foo))))

(ert-deftest pewcfg-core-test-normalize-pair ()
  (should (equal '(foo bar) (pewcfg-normalize-pair '(foo . bar)))))

(ert-deftest pewcfg-core-test-normalize-first-two ()
  (should (equal '(foo bar) (pewcfg-normalize-first-two '(foo bar baz)))))

(ert-deftest pewcfg-core-test-normalize-single ()
  (should (equal '(foo) (pewcfg-normalize-single 'foo))))

(ert-deftest pewcfg-core-test-until-next-keyword ()
  (should (equal '(:a 4 5 6 :b 7 :c 8 9)
                 (pewcfg-until-next-keyword '(1 2 3 :a 4 5 6 :b 7 :c 8 9))))
  (should (equal nil (pewcfg-until-next-keyword nil)))
  (should (equal nil (pewcfg-until-next-keyword '(1 2 3 4 5)))))

(ert-deftest pewcfg-core-test-slice-keyword-segments ()
  (should (equal '((:a 4 5 6) (:b 7) (:c 8 9))
                 (pewcfg-slice-keyword-segments '(1 2 3 :a 4 5 6 :b 7 :c 8 9))))
  (should (equal nil (pewcfg-slice-keyword-segments nil)))
  (should (equal nil (pewcfg-slice-keyword-segments '(1 2 3 4 5))))
  (should (equal '((:a 4 5 6))
                 (pewcfg-slice-keyword-segments '(1 2 3 :a 4 5 6)))))

(ert-deftest pewcfg-core-test-tokey ()
  (should (equal (kbd "C-c C-c") (pewcfg-tokey "C-c C-c")))
  (should (equal [tab] (pewcfg-tokey [tab]))))

;;; Custom theme
(ert-deftest pewcfg-core-test-enable-custom-theme ()
  ;; Enable to make the custom theme appear in `custom-enabled-themes'.
  (enable-theme pewcfg-custom-theme)
  (disable-theme pewcfg-custom-theme)
  (setq unittest-var 'foo)
  (let ((test-result (list unittest-var)))                               ;; foo
    (custom-theme-set-variables pewcfg-custom-theme '(unittest-var 'bar))
    (pewcfg-enable-custom-theme)
    (push unittest-var test-result)                                      ;; bar
    (enable-theme pewcfg-custom-theme)
    (disable-theme pewcfg-custom-theme)
    (push unittest-var test-result)                                      ;; foo
    (pewcfg-enable-custom-theme)
    (push unittest-var test-result)                                      ;; bar
    (should (equal '(foo bar foo bar) (nreverse test-result)))))

;;; Keyword application
(ert-deftest pewcfg-core-test-apply-keyword ()
  (should (equal '((foo foovalue)
                   (bar barvalue))
                 (pewcfg-apply-keyword :unittest
                                       '(foo foovalue)
                                       '(bar barvalue))))
  (should-error (pewcfg-apply-keyword :foo '())))

;;; pewcfg macro expansion
(ert-deftest pewcfg-core-test-pewcfg-happy-path ()
  (should-expand-to (pewcfg
                      :unittest
                      (foo foovalue)
                      (bar barvalue))
                    `(progn (foo foovalue) (bar barvalue))))

(ert-deftest pewcfg-core-test-pewcfg-errors ()
  (should-error (macroexpand '(pewcfg (blah) :unittest (foo foovalue))))
  (should-error (macroexpand '(pewcfg))))

;;; :custom
(ert-deftest pewcfg-core-test-custom-normalize ()
  (should-match (pewcfg--normalize-:custom '((foo foovalue "foodoc")
                                             (bar barvalue)))
                `(('(foo foovalue nil nil "foodoc")
                   '(bar barvalue nil nil "Set by pewcfg:custom")))))

(ert-deftest pewcfg-core-test-custom-generate ()
  (should-match (pewcfg--generate-:custom `'(foo foovalue nil nil "foodoc")
                                          `'(bar barvalue nil nil nil))
                `((let ((custom--inhibit-theme-enable nil))
                    (custom-theme-set-variables
                     'pewcfg
                     '(foo foovalue nil nil "foodoc")
                     '(bar barvalue nil nil nil))))))

;;; :customize
(ert-deftest pewcfg-core-test-customize-normalize ()
  (should-match (pewcfg--normalize-:customize '((foo foovalue "foodoc")
                                                (bar barvalue "bardoc")))
                `((foo foovalue "foodoc")
                  (bar barvalue "bardoc"))))

(ert-deftest pewcfg-core-test-customize-generate ()
  (should-match (pewcfg--generate-:customize 'foo 'foovalue "comment")
                `((customize-set-variable 'foo foovalue "comment")))
  (should-match (pewcfg--generate-:customize 'foo 'foovalue)
                `((customize-set-variable 'foo foovalue "Set by pewcfg:customize"))))

;;; :setq
(ert-deftest pewcfg-core-test-setq-normalize ()
  (should-match (pewcfg--normalize-:setq '((foo foovalue "foodoc")
                                           (bar barvalue "bardoc")))
                `((foo foovalue bar barvalue))))

(ert-deftest pewcfg-core-test-setq-generate ()
  (should-match (pewcfg--generate-:setq 'foo 'foovalue 'bar 'barvalue)
                `((setq foo foovalue bar barvalue))))

;;; :setq-default
(ert-deftest pewcfg-core-test-setq-default-normalize ()
  (should-match (pewcfg--normalize-:setq-default '((foo foovalue "foodoc")
                                                   (bar barvalue "bardoc")))
                `((foo foovalue bar barvalue))))

(ert-deftest pewcfg-core-test-setq-default-generate ()
  (should-match (pewcfg--generate-:setq-default 'foo 'foovalue 'bar 'barvalue)
                `((setq-default foo foovalue bar barvalue))))

;;; :bind
(ert-deftest pewcfg-core-test-bind-normalize ()
  (should-match (pewcfg--normalize-:bind '((foo-map
                                            ("a" . func1)
                                            ("b" . func2))))
                `((foo-map
                   ("a" . func1)
                   ("b" . func2)))))

(ert-deftest pewcfg-core-test-bind-generate ()
  (should-match (pewcfg--generate-:bind 'foo-map
                                        '("a" . func1)
                                        '("b" . func2))
                `((bind-keys :map foo-map ("a" . func1) ("b" . func2))))
  (should-match (pewcfg--generate-:bind 'foo-map)
                `((bind-keys :map foo-map))))

;;; :map
(ert-deftest pewcfg-core-test-map-normalize ()
  (should-match (pewcfg--normalize-:map '((foo-map
                                           ("a" . func1)
                                           ("b" . func2))))
                `((foo-map
                   ("a" . func1)
                   ("b" . func2)))))

(ert-deftest pewcfg-core-test-map-generate ()
  (should-match (pewcfg--generate-:map 'foo-map
                                       '("a" . func1)
                                       '("b" . func2))
                `((define-prefix-command 'foo-map)
                  (bind-keys :map foo-map ("a" . func1) ("b" . func2)))))

(ert-deftest pewcfg-core-test-map-generate-with-parent ()
  (should-match (pewcfg--generate-:map 'foo-map
                                       :parent 'parent-map
                                       '("a" . func1))
                `((define-prefix-command 'foo-map)
                  (set-keymap-parent foo-map parent-map)
                  (bind-keys :map foo-map ("a" . func1)))))

;;; :transient
(ert-deftest pewcfg-core-test-transient-normalize ()
  (should-match (pewcfg--normalize-:transient '((command ("a" . func1)
                                                         ("b" . func2))))
                `((command ("a" . func1) ("b" . func2)))))

(ert-deftest pewcfg-core-test-transient-generate ()
  (should-match (pewcfg--generate-:transient 'command
                                             '("a" . func1)
                                             '("b" . func2))
                `((define-prefix-command 'command-map)
                  (bind-keys :map command-map ("a" . func1) ("b" . func2))
                  (define-key command-map ,_ #'keyboard-quit)
                  (defun command (arg) . ,_)
                  (defun command-repeat () . ,_))))

(ert-deftest pewcfg-core-test-transient-generate-with-parent ()
  (should-match (pewcfg--generate-:transient 'command
                                             :parent 'parent-map
                                             '("a" . func1)
                                             '("b" . func2))
                `((define-prefix-command 'command-map)
                  (set-keymap-parent command-map parent-map)
                  (bind-keys :map command-map ("a" . func1) ("b" . func2))
                  (define-key command-map ,_ #'keyboard-quit)
                  (defun command (arg) . ,_)
                  (defun command-repeat () . ,_))))

;;; :toggle
(ert-deftest pewcfg-core-test-toggle-normalize ()
  (should-match (pewcfg--normalize-:toggle '((foo . foovalue)
                                             (bar . barvalue)))
                `((foo foovalue)
                  (bar barvalue))))

(ert-deftest pewcfg-core-test-toggle-generate ()
  (should-match (pewcfg--generate-:toggle 'foo '(v1 v2 v3))
                `((defvar pew-toggle-foo '(-1 v1 v2 v3) . ,_)
                  (defun pew-toggle-foo () . ,_)))
  (should-match (pewcfg--generate-:toggle 'foo)
                `((defvar pew-toggle-foo '(-1 t nil) . ,_)
                  (defun pew-toggle-foo () . ,_))))

;;; :face
(ert-deftest pewcfg-core-test-face-normalize ()
  (should-match (pewcfg--normalize-:face '((foo
                                            :family "bar"
                                            :weight normal
                                            :height 120
                                            :width normal)))
                `((foo
                   :family "bar"
                   :weight normal
                   :height 120
                   :width normal))))

(ert-deftest pewcfg-core-test-face-generate ()
  (should-match (pewcfg--generate-:face 'foo
                                        :family "bar"
                                        :weight 'normal
                                        :height 120
                                        :width 'normal)
                `((set-face-attribute 'foo nil
                                      :family "bar"
                                      :weight 'normal
                                      :height 120
                                      :width 'normal))))

;;; :property
(ert-deftest pewcfg-core-test-property-normalize ()
  (should-match (pewcfg--normalize-:property '((foo (p1 . v1) (p2 . v2))))
                `((foo (p1 . v1) (p2 . v2)))))

(ert-deftest pewcfg-core-test-property-generate ()
  (should-match (pewcfg--generate-:property 'foo '(p1 . v1) '(p2 . v2))
                `((put 'foo 'p1 v1)
                  (put 'foo 'p2 v2))))

;;; :hook
(ert-deftest pewcfg-core-test-hook-normalize ()
  (should-match (pewcfg--normalize-:hook '((foo-hook . func)))
                `((foo-hook func))))

(ert-deftest pewcfg-core-test-hook-generate ()
  (should-match (pewcfg--generate-:hook 'foo-hook 'func)
                `((add-hook 'foo-hook #'func))))

;;; :automode
(ert-deftest pewcfg-core-test-automode-normalize ()
  (should-match (pewcfg--normalize-:automode '(("matcher regex" . foo-mode)))
                `(("matcher regex" foo-mode))))

(ert-deftest pewcfg-core-test-automode-generate ()
  (should-match (pewcfg--generate-:automode "matcher regex" 'foo-mode)
                `((add-to-list 'auto-mode-alist '("matcher regex" . foo-mode)))))

;;; :eval
(ert-deftest pewcfg-core-test-eval-normalize ()
  (should-match (pewcfg--normalize-:eval '((foo bar)))
                `(((foo bar)))))

(ert-deftest pewcfg-core-test-eval-generate ()
  (should-match (pewcfg--generate-:eval '(foo bar))
                `((foo bar))))

;;; :eval-after
(ert-deftest pewcfg-core-test-eval-after-normalize ()
  (should-match (pewcfg--normalize-:eval-after '((foo (bar a) (baz b))))
                `((foo (bar a) (baz b)))))

(ert-deftest pewcfg-core-test-eval-after-generate ()
  (should-match (pewcfg--generate-:eval-after 'foo '(bar a) '(baz b))
                `((with-eval-after-load 'foo (bar a) (baz b)))))

;;; :vcpkg
(ert-deftest pewcfg-core-test-vcpkg-normalize ()
  (should-match (pewcfg--normalize-:vcpkg '(("foobar/foo" "master")))
                `(("foobar/foo" "master"))))

(ert-deftest pewcfg-core-test-vcpkg-generate ()
  (should-match (pewcfg--generate-:vcpkg "foobar/foo" "master")
                `((unless (package-installed-p 'foo)
                    (package-vc-install
                     (list 'foo :url "https://www.github.com/foobar/foo"
                           :branch "master" :vc-backend 'Git))))))

(provide 'test-pewcfg-core)
;;; test-pewcfg-core.el ends here
