;;; fzf.el --- Integrate fzf in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ghostel)

(defvar fzf-default-term 'ghostel
  "Default terminal backend.")

(defvar fzf-terms
  '((vterm fzf--vterm-create-buffer fzf--vterm-execute)
    (ghostel fzf--ghostel-create-buffer fzf--ghostel-execute))
  "Alist to specify terminal backend handlers.
Each element is of the form (TERM . (CREATION-FUNC EXECUTION-FUNC))")

(defun fzf--vterm-create-buffer (name)
  (vterm name))

(defun fzf--vterm-execute (command)
  (vterm-send-string command)
  (vterm-send-return))

(defun fzf--ghostel-create-buffer (name)
  (let ((ghostel-buffer-name name))
    (ghostel)))

(defun fzf--ghostel-execute (command)
  (ghostel-send-string command)
  (ghostel-send-key "return"))

(defun fzf-find-file (directory &optional term)
  "Use fzf to find file in the given DIRECTORY.

Dependencies:
  - fzf (must in PATH)
  - vterm (Emacs plugin)"
  (interactive "DSearch in: ")
  (let* ((name "fzf-find-file")
         (cache (make-temp-file (format "%s-" name)))
         (command (format "exec fzf >%s" cache))
         (callback (lambda ()
                     (with-temp-buffer
                       (insert-file-contents-literally cache)
                       (delete-file cache)
                       (let ((result (string-trim (buffer-string))))
                         (when result
                           (find-file (expand-file-name result directory)))))))
         (handlers (cdr (assq (or term fzf-default-term) fzf-terms)))
         (default-directory directory)
         (buffer (save-excursion (funcall (car handlers) name))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook callback nil 'local)
      (funcall (cadr handlers) command))))

(provide 'fzf)
;;; fzf.el ends here
