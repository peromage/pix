;;; elpa-avy.el --- Word jumping -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package avy
  :straight t
  :bind
  ( :map pewkey-utility-map
    ("f"   . avy-goto-char)
    ("F" . avy-goto-line)) )

(provide 'elpa-avy)
;;; elpa-avy.el ends here
