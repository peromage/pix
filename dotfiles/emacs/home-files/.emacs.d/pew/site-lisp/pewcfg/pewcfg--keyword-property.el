;;; pewcfg--keyword-property.el --- :property -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:property (forms)
  "Normalize function for ':property'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:property (symbol &rest properties)
  "Set SYMBOL's PROPERTIES.
Where SYMBOL is the name of the symbol and PROPS is an alist whose element is of
the form:
  (PROP . VAL)
PROP is the symbol of the property and VAL is the value to set with."
  (declare (indent 1))
  `(,@(mapcar (lambda (prop)
                `(put ',symbol ',(car prop) ,(cdr prop)))
              properties)))

(provide 'pewcfg--keyword-property)
;;; pewcfg--keyword-property.el ends here
