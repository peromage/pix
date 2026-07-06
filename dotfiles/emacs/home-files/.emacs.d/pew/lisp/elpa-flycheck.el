;;; elpa-flycheck.el --- Alternative to flymake -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :straight t
  :commands (global-flycheck-mode flycheck-mode))

(provide 'elpa-flycheck)
;;; elpa-flycheck.el ends here
