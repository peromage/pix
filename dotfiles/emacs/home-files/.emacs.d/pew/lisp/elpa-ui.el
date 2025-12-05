;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these packages

;; Colors schemes
(use-package doom-themes :defer t)
(use-package spacemacs-theme :defer t)
(use-package dracula-theme :defer t)
(use-package moe-theme :defer t)
(use-package catppuccin-theme :defer t)
(use-package monokai-theme :defer t)
;; From https://protesilaos.com/
(use-package modus-themes :defer t)
(use-package ef-themes :defer t)
;; Fonts and icons
(use-package all-the-icons :defer t)
(use-package nerd-icons :defer t)

;;; Modelines

(use-package doom-modeline
  :disabled
  :demand t
  :custom
  (doom-modeline-height 1)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-unicode-fallback nil)
  :config
  (doom-modeline-mode 1)
  (pewcfg :toggle
          (doom-modeline-unicode-fallback)))

(use-package prot-modeline
  :ensure nil ;; site-lisp
  :demand t)

;;; Misc

(use-package spacious-padding
  :demand t
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths '( :internal-border-width 3
                              :header-line-width 3
                              :mode-line-width 3
                              :tab-width 3
                              :right-divider-width 15
                              :scroll-bar-width 6
                              :fringe-width 9 ))
  :config
  (spacious-padding-mode 1))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package kind-icon
  :disabled
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Setup functions for convenience
(defun pew-install-fonts ()
  (interactive)
  (all-the-icons-install-fonts :silent)
  (nerd-icons-install-fonts :silent))

;; Default looking
(pewlib-load-theme 'modus-vivendi)

(provide 'elpa-ui)
;;; elpa-ui.el ends here
