;;; elpa-ui-doom-modeline.el --- Doom modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :straight t
  :demand t

  :custom
  (doom-modeline-height 1)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-unicode-fallback nil)

  :config
  (doom-modeline-mode 1)
  (pewcfg
    :toggle
    (doom-modeline-unicode-fallback)))

(provide 'elpa-ui-doom-modeline)
;;; elpa-ui-doom-modeline.el ends here
