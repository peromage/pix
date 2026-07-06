;;; elpa-keycast.el --- On-screen key stroke display -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package keycast
  :straight t
  :commands
  (keycast-log-mode
   keycast-tab-bar-mode
   keycast-header-line-mode
   keycast-mode-line-mode)

  :custom
  (keycast-mode-line-format "%2s%k%c%R")
  (keycast-mode-line-window-predicate 'mode-line-window-selected-p) ;; Show in current window
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-tab-bar-format "%k%c%R")
  (keycast-header-line-format "%k%c%R")
  (keycast-header-line-remove-tail-elements nil)

  :config
  (setq keycast-substitute-alist `((self-insert-command "." "Typing...")
                                   (mouse-event-p nil)
                                   (mouse-movement-p nil)
                                   (mouse-set-point nil)
                                   (mouse-drag-region nil)
                                   (mwheel-scroll nil)
                                   (handle-select-window nil)
                                   ,@keycast-substitute-alist)))

(provide 'elpa-keycast)
;;; elpa-keycast.el ends here
