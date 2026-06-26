;;; elpa-ai.el --- AI related packages -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Based on the README, Node.js v18+ must be installed as a prerequisite.
;; For the first time installation, remember to invoke `copilot-install-server'
;; to install corresponding NPM package.

;; Known issues:
;;
;; - Cursor sometimes jumps to the end of Copilot prompt instead of the
;; beginning.  As a result, the `copilot-completion-map' will not take effective.
;; Workaround: Type the first a few letters to move the cursor to the right position.
(use-package copilot
  :straight t
  :commands (copilot-mode global-copilot-mode)

  :bind
  ( :map copilot-completion-map
    ;; Avoid conflict with regular completion
    ("M-n" . copilot-next-completion)
    ("M-p" . copilot-previous-completion)
    ("M-k" . copilot-clear-overlay)
    ("M-j" . copilot-accept-completion)
    ("M-f" . copilot-accept-completion-by-word)
    ("M-e" . copilot-accept-completion-by-line) ))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")

  :bind
  ( :map pew-M-l-map
    ("d" . claude-code-ide-menu))

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

(provide 'elpa-ai)
;;; elpa-ai.el ends here
