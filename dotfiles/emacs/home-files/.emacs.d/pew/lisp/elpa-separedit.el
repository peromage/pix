;;; elpa-separedit.el --- Separate editing in other buffer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package separedit
  :straight t
  :bind
  ( :map pewkey-utility-map
    ("'" . separedit-dwim)) )

(provide 'elpa-separedit)
;;; elpa-separedit.el ends here
