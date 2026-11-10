;;; pewcfg--keyword-setq-default.el --- :setq-default -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:setq-default (forms)
  "Normalize function for ':setq-default'."
  (list (mapcan #'pewcfg-normalize-first-two forms)))

(defun pewcfg--generate-:setq-default (variable value &rest pairs)
  "Simple wrapper of `setq-default'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
PAIRS is the rest of the var-val pairs"
  (declare (indent 0))
  `((setq-default ,variable ,value ,@pairs)))

(provide 'pewcfg--keyword-setq-default)
;;; pewcfg--keyword-setq-default.el ends here
