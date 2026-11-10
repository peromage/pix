;;; pewcfg--keyword-eval-after.el --- :eval-after -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:eval-after (forms)
  "Normalize function for ':eval-after'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:eval-after (feature &rest forms)
  "Evaluate FORMS after a FEATURE is loaded."
  (declare (indent 1))
  `((with-eval-after-load ',feature ,@forms)))

(provide 'pewcfg--keyword-eval-after)
;;; pewcfg--keyword-eval-after.el ends here
