;;; pewcfg--keyword-automode.el --- :automode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:automode (forms)
  "Normalize function for ':automode'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:automode (regexp mode &optional fallthrough)
  "Set `auto-mode-alist'.
REGEXP is a string of regex.
MODE is a symbol of the mode.
FALLTHROUGH can be anything that is non-nil so it will continue matching when
matched with matched suffix removed."
  (declare (indent 0))
  `((add-to-list 'auto-mode-alist (list ,regexp ,mode ,fallthrough))))

(provide 'pewcfg--keyword-automode)
;;; pewcfg--keyword-automode.el ends here
