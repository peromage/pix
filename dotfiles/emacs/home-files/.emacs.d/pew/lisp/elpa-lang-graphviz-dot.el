;;; elpa-lang-graphviz-dot.el --- Graphviz Dot syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package graphviz-dot-mode
  :straight t
  :mode
  (("\\.dot\\'" . graphviz-dot-mode)
   ("\\.gv\\'" . graphviz-dot-mode))

  :custom
  (graphviz-dot-indent-width 4)

  :init
  ;; Graphviz `org-mode' support
  (pewcfg
    :eval-after
    (org
     (pew-org-add-src-lang-modes '(("dot" . graphviz-dot)
                                   ("gv" . graphviz-dot)))
     (pew-org-add-babel-load-languages '((dot . t))))))

(provide 'elpa-lang-graphviz-dot)
;;; elpa-lang-graphviz-dot.el ends here
