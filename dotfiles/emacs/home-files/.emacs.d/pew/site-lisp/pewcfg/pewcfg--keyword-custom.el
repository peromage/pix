;;; pewcfg--keyword-custom.el --- :custom -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Synthetic theme for custom
(defvar pewcfg-custom-theme 'pewcfg
  "The custom theme symbol used by ':custom'.")

(defun pewcfg-enable-custom-theme ()
  "Declare a synthetic theme for custom variables.
Necessary in order to avoid having those variables saved by custom.el.
After enabling, remove the synthetic theme from the enabled themes, so iterating
over them to disable-all-themes won't disable it."
  (interactive)
  (unless (memq pewcfg-custom-theme custom-known-themes)
    (eval `(deftheme ,pewcfg-custom-theme)))
  (enable-theme pewcfg-custom-theme)
  (setq custom-enabled-themes (remq pewcfg-custom-theme custom-enabled-themes)))

(pewcfg-enable-custom-theme)

(defun pewcfg--normalize-:custom (forms)
  "Normalize function for ':custom'."
  (list (mapcar (lambda (form)
                  `'(,(elt form 0) ,(elt form 1) nil nil ,(let ((comment (elt form 2)))
                                                            (if comment comment "Set by pewcfg:custom"))))
                forms)))

(defun pewcfg--generate-:custom (&rest args)
  "Bind variable values to a custom theme `pewcfg-custom-theme'."
  `((let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables ',pewcfg-custom-theme ,@args))))

(provide 'pewcfg--keyword-custom)
;;; pewcfg--keyword-custom.el ends here
