;;; elpa-lang-python.el --- Python syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; TS mode included already
(use-package python-mode
  :straight t
  :defer t
  :hook ((python-ts-mode . pew-python-ts-mode-setup)
         (python-ts-mode . indent-bars-mode))

  :preface
  (defun pew-python-ts-mode-setup ()
    (setq-local python-indent-offset 4)))

(provide 'elpa-lang-python)
;;; elpa-lang-python.el ends here
