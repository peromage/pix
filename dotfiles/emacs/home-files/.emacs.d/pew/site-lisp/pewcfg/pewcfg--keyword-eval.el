;;; pewcfg--keyword-eval.el --- :eval -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:eval (forms)
  "Normalize function for ':eval'."
  (mapcar #'pewcfg-normalize-single forms))

(defun pewcfg--generate-:eval (form)
  "Simply return the FORM."
  (declare (indent 0))
  `(,form))

(provide 'pewcfg--keyword-eval)
;;; pewcfg--keyword-eval.el ends here
