;;; elpa-lang-nix.el --- Nix syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package nix-mode
  :straight t
  :defer t)

(use-package nix-ts-mode
  :straight t
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . pew-nix-ts-mode-setup)

  :custom
  (nix-ts-mode-indent-offset 2)

  :preface
  (defun pew-nix-ts-mode-setup ()
    (pew-treesit-add-indent-rules 'nix
      '(;; NOTE: query only takes 2 nodes (parent and child) and the node
        ;; to be indented needs to be captured.
        ((query ((inherited_attrs) @attr)) parent-bol nix-ts-mode-indent-offset)
        ((query ((inherited_attrs (_) @attr))) grand-parent nix-ts-mode-indent-offset)
        ((query ((if_expression ["then" "else"] @branch))) parent 0)
        ((query ((let_expression "in" body: (_) @body))) prev-line nix-ts-mode-indent-offset)
        ((query ((let_expression "in" @in))) parent-bol 0)))))

(provide 'elpa-lang-nix)
;;; elpa-lang-nix.el ends here
