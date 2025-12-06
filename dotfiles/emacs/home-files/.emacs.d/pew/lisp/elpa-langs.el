;;; elpa-langs.el --- Language mode supports -*- lexical-binding: t; -*-
;;; Commentary:

;; To get started, the grammar libraries must installed for the first time.
;; Use 'treesit-langs-install-grammars' to install a pre-built pack of grammar
;; from `treesit-langs.el'.
;;
;; For anything that is missing from above, use `treesit-install-language-grammar'
;; to install additional grammar.  Note that this requires local compilation.
;;
;; To check if a grammar is supported, use `treesit-language-available-p'.
;;
;; To debug/customize indentation by leveraging syntax tree, set
;; `treesit--indent-verbose' to non-nil to show current matched indentation rule
;; at the echo area.  Additionally, turn on `treesit-explore-mode' to view the
;; tree.
;; NOTE: The rule list is read in sequence so more specific matchers should be
;; put at the front.  For example, n-p-gp should be earlier than parent-is.
;;
;; See `treesit-simple-indent-presets' for matcher and anchor definitions.
;; See also: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html

;;; Code:

;; Treesit {

;; Emacs builtin tree-sitter
(use-package treesit
  :straight nil

  :custom
  (treesit-font-lock-level 4) ;; Maximize font rendering

  (treesit-language-source-alist
   '((elisp "https://github.com/Wilfred/tree-sitter-elisp" "main")
     (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl" "master")))

  ;; Map traditional major modes to tree-sitter major modes
  (major-mode-remap-alist
   '((c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (nix-mode . nix-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (sh-mode . bash-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (toml-mode . toml-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)))

  :config
  (pewcfg
    :toggle
    ;; Debug indent rules when `treesit-explore-mode' is on
    (treesit--indent-verbose)))


;; Grammar pack
(use-package treesit-langs
  :straight (:type git :host github :repo "emacs-tree-sitter/treesit-langs" :branch "main")
  :commands (treesit-langs-major-mode-setup treesit-langs-install-grammars))

;;; Auxiliary packages

(use-package treesit-fold
  :straight (:type git :host github :repo "emacs-tree-sitter/treesit-fold" :branch "master")

  :config
  ;; (global-treesit-fold-indicators-mode) ;; Not working in terminal
  (global-treesit-fold-mode 1)) ;; Automatically adds folding functions to `evil-fold-list'.

;; } Treesit

;; Oneliners {

(use-package vimrc-mode :defer t)
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)
(use-package fish-mode :defer t)
(use-package csharp-mode :defer t)
(use-package powershell :defer t)
(use-package markdown-mode :defer t)

;; } Oneliners

;; C/C++ {

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
  (defun pew-c-ts-mode-indent-style ()
    "Customized indentation rules.
See: https://www.reddit.com/r/emacs/comments/1bgdw0y/custom_namespace_indentation_in_ctsmode"
    (nconc '(;; Do not indent preprocessor directives
             ((node-is "preproc") column-0 0)
             ;; Do not indent namespace children
             ((n-p-gp nil nil "namespace_definition") grand-parent 0))
           ;; Base rule
           (alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

  (defun pew-cc-ts-mode-setup ()
    "Common C/C++ TS mode preference."
    (setq-local c-ts-mode-indent-offset 4)
    (setq-local c-ts-mode-indent-style #'pew-c-ts-mode-indent-style)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (setq-local tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
    (setq-local adaptive-fill-mode nil)))

;; } C/C++

;; Python{

;; TS mode included already
(use-package python-mode
  :defer t
  :hook (python-ts-mode . pew-python-ts-mode-setup)

  :preface
  (defun pew-python-ts-mode-setup ()
    (setq-local python-indent-offset 4)))

;; } Python

;; Nix {

(use-package nix-mode :defer t)


(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode)

  :custom
  (nix-ts-mode-indent-offset 2)

  :config
  (setf (alist-get 'nix nix-ts-mode-indent-rules)
        (nconc '(;; NOTE: query only takes 2 nodes (parent and child) and the node
                 ;; to be indented needs to be captured.
                 ((query ((inherited_attrs) @attr)) parent-bol nix-ts-mode-indent-offset)
                 ((query ((inherited_attrs (_) @attr))) grand-parent nix-ts-mode-indent-offset)
                 ((query ((if_expression ["then" "else"] @branch))) parent 0)
                 ((query ((let_expression "in" body: (_) @body))) prev-line nix-ts-mode-indent-offset)
                 ((query ((let_expression "in" @in))) parent-bol 0))
               (alist-get 'nix nix-ts-mode-indent-rules))))

;; } Nix

;; UML {

(use-package plantuml-mode
  :mode
  (("\\.puml\\'" . plantuml-mode)
   ("\\.plantuml\\'" . plantuml-mode))

  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)

  :init
  ;; `org-mode' support
  (pewcfg
    :eval-after
    (org
     (setq org-plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
     (setq org-plantuml-exec-mode 'jar)
     (pew-org-add-src-lang-modes '(("plantuml" . plantuml)))
     (pew-org-add-babel-load-languages '((plantuml . t))))))


(use-package graphviz-dot-mode
  :mode
  (("\\.dot\\'" . graphviz-dot-mode)
   ("\\.gv\\'" . graphviz-dot-mode))

  :custom
  (graphviz-dot-indent-width 4)

  :init
  ;; Graphviz `org-mode' support
  (pewcfg
    :eval-after
    (org
     (pew-org-add-src-lang-modes '(("dot" . graphviz-dot)
                                   ("gv" . graphviz-dot)))
     (pew-org-add-babel-load-languages '((dot . t))))))


(use-package mermaid-mode
  :mode ("\\.mmd\\'" . mermaid-mode))


;; Mermaid `org-mode' support
(use-package ob-mermaid
  :defer t

  :init
  (pewcfg
    :eval-after
    (org
     (pew-org-add-src-lang-modes '(("mermaid" . mermaid)))
     (pew-org-add-babel-load-languages '((mermaid . t))))))

;; } UML

;; Misc {

(use-package cmake-mode
  :defer t

  :custom
  cmake-tab-width 4)


(use-package lua-mode
  :defer t

  :custom
  (lua-indent-level 2))


(use-package kdl-ts-mode
  :straight (:type git :host github :repo "dataphract/kdl-ts-mode" :branch "main")
  :mode ("\\.kdl\\'" . kdl-ts-mode)

  :custom
  (kdl-ts-mode-indent-offset 2))

;; } Misc

(provide 'elpa-langs)
;;; elpa-langs.el ends here
