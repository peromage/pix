;;; elpa-ui-fonts.el --- Fonts and icons -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons
  :straight t
  :defer t)

(use-package nerd-icons
  :straight t
  :defer t)

(use-package nerd-icons-completion
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Setup functions for convenience
(defun pew-install-fonts ()
  (interactive)
  (all-the-icons-install-fonts :silent)
  (nerd-icons-install-fonts :silent))

(provide 'elpa-ui-fonts)
;;; elpa-ui-fonts.el ends here
