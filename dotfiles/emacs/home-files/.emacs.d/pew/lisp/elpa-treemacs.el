;;; elpa-treemacs.el --- File explorer in side window -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :straight t
  :commands treemacs
  :hook (treemacs-mode . pew-treemacs-mode-setup)

  :bind
  ( :map treemacs-mode-map
    ("j" . treemacs-find-file) )

  :custom
  (treemacs-wrap-around nil)
  (treemacs-eldoc-display 'detailed)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory nil)

  :preface
  (defun pew-treemacs-mode-setup ()
    "`treemacs-mode' initialization."
    (display-line-numbers-mode -1))

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(provide 'elpa-treemacs)
;;; elpa-treemacs.el ends here
