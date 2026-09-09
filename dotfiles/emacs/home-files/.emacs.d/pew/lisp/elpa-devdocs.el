;;; elpa-devdocs.el --- Devdocs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package devdocs
  :straight t

  :bind
  ( :map pewkey-utility-map
    ("d" . devdocs-lookup)) )


(provide 'elpa-devdocs)
;;; elpa-devdocs.el ends here
