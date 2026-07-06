;;; elpa-paredit.el --- Parenthesis editing for sexp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :straight t
  :hook
  ((lisp-interaction-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (lisp-data-mode . paredit-mode)))

(provide 'elpa-paredit)
;;; elpa-paredit.el ends here
