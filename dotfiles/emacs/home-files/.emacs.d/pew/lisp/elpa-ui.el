;;; elpa-ui.el --- ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these packages

;; Colors schemes
(use-package doom-themes
  :straight t
  :defer t)

(use-package spacemacs-theme
  :straight t
  :defer t)

(use-package dracula-theme
  :straight t
  :defer t)

(use-package moe-theme
  :straight t
  :defer t)

(use-package catppuccin-theme
  :straight t
  :defer t)

(use-package monokai-theme
  :straight t
  :defer t)

;; From https://protesilaos.com/
(use-package modus-themes
  :straight t
  :defer t)

(use-package ef-themes
  :straight t
  :defer t)

;; Fonts and icons
(use-package all-the-icons
  :straight t
  :defer t)

(use-package nerd-icons
  :straight t
  :defer t)

;;; Modelines

(use-package doom-modeline
  :straight t
  :disabled
  :demand t

  :custom
  (doom-modeline-height 1)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-unicode-fallback nil)

  :config
  (doom-modeline-mode 1)
  (pewcfg
    :toggle
    (doom-modeline-unicode-fallback)))


(use-package prot-modeline
  :straight nil ;; site-lisp
  :demand t)

;;; Misc

(use-package spacious-padding
  :straight t
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
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))


(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))


(use-package nerd-icons-corfu
  :straight t
  :after corfu

  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package kind-icon
  :straight t
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
