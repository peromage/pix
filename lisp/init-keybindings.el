;;; init-keybindings.el --- Keymaps for original features -*- lexical-binding: t -*-
;;; Commentary:

;; Global keymaps respect vanilla Emacs settings

;;; Code:

(pew/global-set-key '(("C-x t SPC" . tab-bar-select-tab-by-name)
                      ("C-x t f" . tab-bar-switch-to-next-tab)
                      ("C-x t b" . tab-bar-switch-to-prev-tab)
                      ("C-x t m" . pew/move-tab-next)
                      ("C-x t M" . pew/move-tab-prev)
                      ("C-x t t" . tab-bar-new-tab)
                      ("C-x t T" . pew/pop-window-in-new-tab)
                      ("C-x t l" . tab-switcher)
                      ([remap next-buffer] . pew/next-buffer)
                      ([remap previous-buffer] . pew/prev-buffer)))

(provide 'init-keybindings)
;;; init-keybindings.el ends here