;;; elpa-lang-cpp.el --- C/C++ syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package cc-mode
  :straight nil

  :hook
  ((c-mode . pew-cc-mode-setup)
   (c++-mode . pew-cc-mode-setup))

  :preface
  ;; Setup functions
  (defun pew-cc-mode-setup ()
    "Common CC mode setup."
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)
    ;; Indentation
    (setq-local indent-tabs-mode nil)
    (setq-local c++-tab-always-indent t)
    (setq-local c-basic-offset 4)
    (setq-local c-indent-level 4)
    (setq-local tab-width 4)
    (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq-local c-syntactic-indentation t)
    (setq-local c-syntactic-indentation-in-macros t)
    ;; Fill columns
    (setq-local adaptive-fill-mode nil)
    ;; Macro line continuation
    (setq-local c-backslash-column 80)
    (setq-local c-backslash-max-column 160)
    (setq-local c-auto-align-backslashes t)))

(use-package c-ts-mode
  :straight nil

  :hook
  ((c-ts-mode . pew-cc-ts-mode-setup)
   (c++-ts-mode . pew-cc-ts-mode-setup))

  :preface
  (defun pew-cc-ts-mode-setup ()
    "Common C/C++ TS mode preference."
    (setq-local c-ts-mode-indent-offset 4)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq-local adaptive-fill-mode nil)

    ;; For C++
    ;; See: https://www.reddit.com/r/emacs/comments/1bgdw0y/custom_namespace_indentation_in_ctsmode
    (pew-treesit-add-indent-rules 'cpp
      '(;; Do not indent preprocessor directives
        ((node-is "preproc") column-0 0)
        ;; Do not indent namespace children
        ((n-p-gp nil nil "namespace_definition") grand-parent 0)
        ((node-is "access_specifier") parent-bol 0)
        ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
        ((parent-is "init_declarator") parent-bol c-ts-mode-indent-offset)
        ((match "parameter_declaration" "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
        ((match "parameter_declaration" "parameter_list" nil nil nil) (nth-sibling 1)  0)))))

(provide 'elpa-lang-cpp)
;;; elpa-lang-cpp.el ends here
