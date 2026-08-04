;;; fzf.el --- Integrate fzf in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar fzf-terms
  '((vterm fzf--vterm-create-buffer fzf--vterm-execute))
  "Alist to specify terminal backend handlers.
Each element is of the form (TERM . (TERM-CREATION-FUNC TERM-EXECUTION-FUNC))")

(defvar fzf-default-term 'vterm
  "Default terminal backend.")

(defun fzf--vterm-create-buffer (name)
  (let ((default-directory directory))
    (vterm name)))

(defun fzf--vterm-execute (command)
  (vterm-send-string command)
  (vterm-send-return))

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
         (buffer (save-excursion (funcall (car handlers) name))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook callback nil 'local)
      (funcall (cadr handlers) command))))

(provide 'fzf)
;;; fzf.el ends here
