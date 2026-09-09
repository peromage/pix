;;; elpa-claude-code-ide.el --- Claude agent -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")

  :bind
  ( :map pewkey-utility-map
    ("k" . claude-code-ide-menu) )

  :custom
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  (claude-code-ide-diagnostics-backend 'flymake)
  (claude-code-ide-enable-execute-code t)
  (claude-code-ide-cli-extra-flags "--model opus")
  ;; Vterm related
  (claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-vterm-anti-flicker t)
  (claude-code-ide-vterm-render-delay 0.01)

  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'elpa-claude-code-ide)
;;; elpa-claude-code-ide.el ends here
