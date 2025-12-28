;;; pewlib-editor.el --- Editting functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; In buffer
(defun /p/delete-trailing-whitespaces ()
  "Clear trailing whitespaces in current buffer."
  (delete-trailing-whitespace (point-min) (point-max)))

(defun /p/indent-space-in-buffer ()
  "Use spaces for indentation in buffer."
  (setq-local indent-tabs-mode nil
              tab-width 4))

(defun /p/terminal-mode-setup ()
  "Common setup for terminal/shell modes."
  (setq-local word-wrap nil
              truncate-lines nil
              truncate-partial-width-windows nil
              show-trailing-whitespace nil
              global-hl-line-mode nil) ;; Needed, blink otherwise
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun /p/text-mode-setup ()
  "Common setup for text processing modes."
  (setq-local line-move-visual t)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (whitespace-mode 1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun /p/prog-mode-setup ()
  "Common setup for programming modes."
  (outline-minor-mode 1)
  (whitespace-mode 1)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (electric-indent-local-mode 1))

(provide 'pewlib-editor)
;;; pewlib-editor.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/p/" . "pewlib-"))
;; End:
