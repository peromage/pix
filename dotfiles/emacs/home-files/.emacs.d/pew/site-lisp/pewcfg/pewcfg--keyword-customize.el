;;; pewcfg--keyword-customize.el --- :customize -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:customize (forms)
  "Normalize function for ':customize'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:customize (variable value &optional comment)
  "Bind a VARIABLE with the VALUE.
Similar to `pewcfg--generate-:custom' while this does not use a custom theme.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
COMMENT is the optional commentary shown in the `customize' interface."
  (declare (indent 0))
  `((customize-set-variable ',variable ,value ,(if comment comment "Set by pewcfg:customize"))))

(provide 'pewcfg--keyword-customize)
;;; pewcfg--keyword-customize.el ends here
