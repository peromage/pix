;;; elpa-git-gutter.el --- Git indicator on fringe -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package git-gutter
  :straight t
  :custom
  (git-gutter:modified-sign "**")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  (git-gutter:unchanged-sign nil)
  (git-gutter:separator-sign nil)
  (git-gutter:update-interval 2)
  (git-gutter:visual-line nil)
  (git-gutter:hide-gutter nil)
  (git-gutter:verbosity 0)

  :custom-face
  (git-gutter:modified   ((t (:foreground  "yellow"       :background  "unspecified"))))
  (git-gutter:added      ((t (:foreground  "green"        :background  "unspecified"))))
  (git-gutter:deleted    ((t (:foreground  "red"          :background  "unspecified"))))
  (git-gutter:unchanged  ((t (:foreground  "unspecified"  :background  "unspecified"))))
  (git-gutter:separator  ((t (:foreground  "unspecified"  :background  "unspecified"))))

  :config
  (global-git-gutter-mode 1))

(provide 'elpa-git-gutter)
;;; elpa-git-gutter.el ends here
