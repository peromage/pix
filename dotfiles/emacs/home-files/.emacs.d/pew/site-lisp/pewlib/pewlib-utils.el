;;; pewlib-utils.el --- Utility commands/functions -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:
(defun shell-command-on-current-file (command &optional placeholder)
  (interactive
   (list (read-shell-command "Shell command on current file: ")
         "{}"))

  (when (string-empty-p command)
    (user-error "No command input"))
  (when (null buffer-file-name)
    (user-error "Not a file"))

  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer? "))
    (save-buffer))

  (let* ((normalized-file-name (shell-quote-argument
                                (file-local-name buffer-file-name)))
         (quoted-placeholder (regexp-quote placeholder))
         (final-command (if (string-match-p quoted-placeholder command)
                            (replace-regexp-in-string quoted-placeholder normalized-file-name command)
                          (concat command " " normalized-file-name))))
    (shell-command final-command)))

(provide 'pewlib-utils)
;;; pewlib-utils.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/p/" . "pewlib-"))
;; End:
