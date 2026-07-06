;;; elpa-yasnippet.el --- Code snippet -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :straight t
  :custom
  (yas-indent-line 'fixed)

  :config
  (pewcfg
    :customize
    (yas-snippet-dirs (cons (expand-file-name "pew/yasnippet" pew-toplevel-dir)
                            (yas-snippet-dirs))))
  (yas-global-mode 1))

(provide 'elpa-yasnippet)
;;; elpa-yasnippet.el ends here
