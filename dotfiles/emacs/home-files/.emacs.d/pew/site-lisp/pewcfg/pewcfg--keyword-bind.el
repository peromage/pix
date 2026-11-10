;;; pewcfg--keyword-bind.el --- :bind -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:bind (forms)
  "Normalize function for ':bind'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:bind (keymap &rest bindings)
  "Bind keys in an existing KEYMAP.
KEYMAP is a symbol of the keymap.
BINDINGS is an alist whose element is:
  (KEY . DEF)
For DEF's definition see `keymap-set'. Additionally, a special syntax
NOTE: Unlike `pewcfg--generate-:map' this macro does not create a new map.  It sets
keybindings in a existing map instead."
  (declare (indent 1))
  (mapcar (lambda (binding) `(keymap-set ,keymap ,(car binding) ,(cdr binding))) bindings))

(provide 'pewcfg--keyword-bind)
;;; pewcfg--keyword-bind.el ends here
