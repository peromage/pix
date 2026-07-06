;;; elpa-treesit.el --- Treesitter -*- lexical-binding: t; -*-
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
     (python-mode . python-ts-mode)
     (js-mode . js-ts-mode)
     (javascript-mode . js-ts-mode)
     (html-mode . html-ts-mode)
     (mhtml-mode . html-ts-mode)
     (css-mode . css-ts-mode)))

  :preface
  (defun pew-treesit-add-indent-rules (lang rules)
    "Add a list of RULES for specific LANG.
Treesit seems to change the way how it indents in recent updates.  It used to be
managed by a specific major mode, for example `c-ts-mode-indent-style' but now
it doesn't seem to work any more. Instead, we need to update
`treesit-simple-indent-rules'.
NOTE: For 'query' matchers, the sexp 'query' won't work unless they are compiled."
    (declare (indent 1))
    (when (assq lang treesit-simple-indent-rules)
      (setf (alist-get lang treesit-simple-indent-rules)
            (nconc (mapcar (lambda (rule)
                             ;; Compile query in rules
                             (if (and (listp (car rule)) (eq 'query (caar rule)))
                                 `((query ,(treesit-query-compile lang (cdar rule))) ,@(cdr rule))
                               rule))
                           rules)
                   (alist-get lang treesit-simple-indent-rules)))))

  (defun pew-treesit-indent ()
    "Same as `treesit-indent' but make it interactive."
    (interactive)
    (treesit-indent))

  (define-minor-mode pew-treesit-debug-mode
    "Turn on treesit inspect and explor mode for current buffer."
    :lighter nil
    (when (not (local-variable-if-set-p 'treesit--indent-verbose))
      (make-variable-buffer-local 'treesit--indent-verbose))
    (cond (pew-treesit-debug-mode
           (setq-local treesit--indent-verbose t)
           (treesit-inspect-mode 1)
           (treesit-explore-mode 1))
          (t
           (setq-local treesit--indent-verbose nil)
           (treesit-inspect-mode -1)
           (treesit-explore-mode -1)))))

;;; Auxiliary packages

(use-package treesit-langs
  :straight (:type git :host github :repo "emacs-tree-sitter/treesit-langs" :branch "main")
  :commands (treesit-langs-major-mode-setup treesit-langs-install-grammars))

(use-package treesit-fold
  :straight (:type git :host github :repo "emacs-tree-sitter/treesit-fold" :branch "master")

  :config
  ;; (global-treesit-fold-indicators-mode) ;; Not working in terminal
  (global-treesit-fold-mode 1)) ;; Automatically adds folding functions to `evil-fold-list'.

(provide 'elpa-treesit)
;;; elpa-treesit.el ends here
