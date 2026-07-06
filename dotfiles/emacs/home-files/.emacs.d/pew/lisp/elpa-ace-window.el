;;; elpa-ace-window.el --- Window movement -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ace-window
  :straight t
  :bind
  ( :map pewkey-utility-map
    ("w" . ace-window)
    ("W" . ace-swap-window)) )

(provide 'elpa-ace-window)
;;; elpa-ace-window.el ends here
