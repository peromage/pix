;;; fzf.el --- Integrate fzf in Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun fzf-find-file (&optional directory)
  "Use fzf to find file in the given DIRECTORY.

Dependencies:
  - fzf (must in PATH)
  - vterm (Emacs plugin)"
  (interactive "DSearch in: ")
  (let* ((cache (make-temp-file "fzf-"))
         (callback (lambda ()
                     (with-temp-buffer
                       (insert-file-contents-literally cache)
                       (delete-file cache)
                       (let ((result (string-trim (buffer-string))))
                         (when result
                           (find-file (expand-file-name result directory)))))))
         (vterm-buffer (save-excursion
                         (let ((default-directory directory))
                           (vterm "fzf-find-file")))))
    (with-current-buffer vterm-buffer
      (add-hook 'kill-buffer-hook callback nil t)
      (vterm-send-string (format "exec fzf >%s" cache))
      (vterm-send-return))))

(provide 'fzf)
;;; fzf.el ends here
