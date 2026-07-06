;;; elpa-vterm.el --- Terminal by libvterm -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :straight t
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pewlib-terminal-mode-setup)

  :bind
  ( :map vterm-mode-map
    ("ESC ESC" . vterm-send-escape) )

  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-keymap-exceptions '("C-z" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x"
                             "M-o" "C-y" "M-y" "M-:"))
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("plink" "/bin/bash")))

  :preface
  (defun pew-vterm-new (shell)
    "Create a new vterm window.
ARG is a prefix argument.  If it is non-nill, a prompt will pop up to allow
users to specify the shell to start with."
    (interactive "sShell: ")
    (unless (boundp 'vterm-shell)
      (defvar vterm-shell))
    (let ((vterm-shell shell))
      (vterm :new))))

(provide 'elpa-vterm)
;;; elpa-vterm.el ends here
