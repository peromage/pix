;;; elpa-ghostel.el --- Terminal by libghostty -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ghostel
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :straight t
  :hook (ghostel-mode . pew-ghostel-mode-setup)

  :bind
  ( :map pewkey-utility-map
    ("t" . ghostel)
    :map ghostel-semi-char-mode-map
    ("C-s" . consult-line)
    :map project-prefix-map
    ("t" . ghostel-project)
    ("T" . ghostel-project-list-buffers))

  :custom
  (ghostel-shell-integration t)
  (ghostel-shell '("sh" "-c" "command -v fish >/dev/null && exec fish -i || exec bash -i"))
  (ghostel-keymap-exceptions '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\" "C-z" "ESC"))
  ;; Set the initial buffer name to match window management regex in `pewlib-buffer-regex-plist'
  ;; The name is then renamed by `ghostel-buffer-name-function' after creation
  (ghostel-buffer-name "*ghostelterm*")

  :preface
  (defun pew-ghostel-mode-setup ()
    (setq-local show-trailing-whitespace nil))

  :config
  (pewlib-add-to-list 'project-switch-commands :end
    '((ghostel-project "Ghostel")
      (ghostel-project-list-buffers "Ghostel buffers"))))

(provide 'elpa-ghostel)
;;; elpa-ghostel.el ends here
