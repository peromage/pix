;;; elpa-utils.el --- Ad-hoc utility declarations -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Search
(use-package rg
  :straight t
  :defer t)

;; Focused view
(use-package olivetti
  :straight t
  :defer t)

;; Cats!!!
(use-package nyan-mode
  :straight t
  :defer t)

(use-package zone-nyan
  :straight t
  :defer t)

;; Colorful parenthesises
(use-package rainbow-delimiters
  :straight t
  :defer t)

;; Colorize color code
(use-package rainbow-mode
  :straight t
  :defer t)

;; Highlight current line
(use-package beacon
  :straight t
  :defer t)

;; Hyperbole
(use-package hyperbole
  :straight t
  :defer t)

;; Useful commands
(use-package crux
  :straight t
  :defer t)

;; Indentation line
(use-package indent-bars
  :straight t
  :defer t)

(provide 'elpa-utils)
;;; elpa-utils.el ends here
