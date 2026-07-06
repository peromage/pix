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
(use-package modus-themes
  :straight t
  :defer t)

(use-package ef-themes
  :straight t
  :defer t)

;; Default looking
(pewlib-load-theme 'modus-vivendi)

(provide 'elpa-ui-themes)
;;; elpa-ui-themes.el ends here
