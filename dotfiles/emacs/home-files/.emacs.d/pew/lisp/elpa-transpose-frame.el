;;; elpa-transpose-frame.el --- Window transposing -*- lexical-binding: t; -*-
;;; Commentary:

;; NOTE: Since Emacs 31 built-in transposing functions are integrated. This
;; package may be removed.

;;; Code:

(use-package transpose-frame
  :straight t
  :bind
  ( :map pewkey-window-map
    ("t" . transpose-frame)
    ("r" . rotate-frame)
    ("f" . flip-frame)
    ("F" . flop-frame)
    ("g" . rotate-frame-clockwise)
    ("G" . rotate-frame-anticlockwise) ))

(provide 'elpa-transpose-frame)
;;; elpa-transpose-frame.el ends here
