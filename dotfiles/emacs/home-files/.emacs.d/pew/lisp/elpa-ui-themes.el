;;; elpa-ui-themes.el --- Theme packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
;; NOTE: Since Emacs 28+ the modus-themes has a built-in copy which could
;; potentially jeopardize the theme loading of modus collection (as this
;; external copy is loaded after). One way to resolve this is to set ':demand t'
;; but I don't want to do that so just a reminder
(use-package modus-themes
  :straight t
  :defer t)

(use-package ef-themes
  :straight t
  :defer t)

(use-package doric-themes
  :straight t
  :defer t)

(use-package standard-themes
  :straight t
  :defer t)

;; Default looking
(pewlib-load-theme 'modus-vivendi)

(provide 'elpa-ui-themes)
;;; elpa-ui-themes.el ends here
