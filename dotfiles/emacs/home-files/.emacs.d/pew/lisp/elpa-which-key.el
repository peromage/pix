;;; elpa-which-key.el --- Key hints -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :straight t
  :commands which-key-mode

  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)

  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

(provide 'elpa-which-key)
;;; elpa-which-key.el ends here
