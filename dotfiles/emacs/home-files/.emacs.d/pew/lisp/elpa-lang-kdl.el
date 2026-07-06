;;; elpa-lang-kdl.el --- KDL syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package kdl-ts-mode
  :straight (:type git :host github :repo "dataphract/kdl-ts-mode" :branch "main")
  :mode ("\\.kdl\\'" . kdl-ts-mode)

  :custom
  (kdl-ts-mode-indent-offset 2))

(provide 'elpa-lang-kdl)
;;; elpa-lang-kdl.el ends here
