;;; elpa-ui-prot-modeline.el --- Modeline from Prot's config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package prot-modeline
  :straight nil ;; site-lisp
  :demand t)

(use-package spacious-padding
  :straight t
  :demand t

  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths '( :internal-border-width 3
                              :header-line-width 3
                              :mode-line-width 3
                              :tab-width 3
                              :right-divider-width 15
                              :scroll-bar-width 6
                              :fringe-width 9 ))

  :config
  (spacious-padding-mode 1))

(provide 'elpa-ui-prot-modeline)
;;; elpa-ui-prot-modeline.el ends here
