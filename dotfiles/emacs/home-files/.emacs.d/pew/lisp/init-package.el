;;; init-package.el --- package management -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'use-package)

;; Use straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;; Use use-package.el for configuration
(setq use-package-always-ensure nil)
(setq use-package-always-defer nil)
(setq use-package-always-demand nil)
(setq use-package-always-pin nil)
(setq use-package-compute-statistics nil)
(setq use-package-verbose nil)

;; Workaround
(defun pew--reload-use-package-custom-theme ()
  "A quick way to enable all the settings from the `use-package' theme.
After enabling, remove the synthetic theme from the enabled themes, so iterating
over them to disable-all-themes won't disable it.
NOTE: This is used as a workaround to fix some variables that are set with
:custom in `use-package'."
  (interactive)
  (enable-theme 'use-package)
  (setq custom-enabled-themes (remq 'use-package custom-enabled-themes))
  (message "[pew] Reloaded use-package custom theme"))
(add-hook 'after-init-hook #'pew--reload-use-package-custom-theme)

(provide 'init-package)
;;; init-package.el ends here
