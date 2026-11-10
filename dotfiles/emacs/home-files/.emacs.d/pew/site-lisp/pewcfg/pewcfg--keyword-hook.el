;;; pewcfg--keyword-hook.el --- :hook -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:hook (forms)
  "Normalize function for ':hook'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:hook (name function &optional depth)
  "Set a FUNCTION to a hook NAME.
NOTE: NAME does not imply suffix '-hook'.
See `add-hook' for parameter."
  (declare (indent 0))
  `((add-hook ',name ,function ,depth)))

(provide 'pewcfg--keyword-hook)
;;; pewcfg--keyword-hook.el ends here
