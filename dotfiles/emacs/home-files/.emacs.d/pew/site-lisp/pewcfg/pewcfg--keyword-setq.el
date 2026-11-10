;;; pewcfg--keyword-setq.el --- :setq -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:setq (forms)
  "Normalize function for ':setq'."
  (list (mapcan #'pewcfg-normalize-first-two forms)))

(defun pewcfg--generate-:setq (variable value &rest pairs)
  "Simple wrapper of `setq'.
VARIABLE is a symbol of the variable.
VALUE will not be evaluate until the expanded form is executed.
PAIRS is the rest of the var-val pairs"
  (declare (indent 0))
  `((setq ,variable ,value ,@pairs)))

(provide 'pewcfg--keyword-setq)
;;; pewcfg--keyword-setq.el ends here
