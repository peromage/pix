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
  (let* ((default-directory directory)
         (name "fzf-find-file")
         (cache (make-temp-file (format "%s-" name)))
         ;; Replace the shell process causing terminal exit once it finishes
         (command (format "exec fzf >%s" cache))
         (callback (lambda ()
                     (let ((fzf-output (with-temp-buffer
                                         (insert-file-contents-literally cache)
                                         (delete-file cache)
                                         (string-trim (buffer-string)))))
                       (unless (string= "" fzf-output)
                         ;; Defer opening the file: running `find-file' inside
                         ;; `kill-buffer-hook' switches `current-buffer', which
                         ;; breaks the terminal backend's own kill-buffer-hook
                         ;; that dereferences buffer-local state (e.g. ghostel's
                         ;; native terminal handle) after us.
                         (run-at-time 0 nil #'find-file
                                      (expand-file-name fzf-output directory))))))
         (handlers (cdr (assq (or term fzf-default-term) fzf-terms)))
         (buffer (save-excursion (funcall (car handlers) name))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook callback nil 'local)
      (funcall (cadr handlers) command))))

(provide 'fzf)
;;; fzf.el ends here
