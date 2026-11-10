;;; pewcfg--keyword-map.el --- :map -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:map (forms)
  "Normalize function for ':map'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:map (keymap &rest args)
  "Create a new KEYMAP with `defvar-keymap' and bind keys in it.
KEYMAP is a symbol of the keymap.
ARGS may begin with any keyword arguments accepted by `defvar-keymap'
\(e.g. :parent, :repeat, :suppress), which are passed through to it verbatim.
The remaining elements are key bindings in the same cons-cell form as in
`pewcfg--generate-:bind':
  (KEY . DEF)
NOTE: Unlike `pewcfg--generate-:bind' this function creates a new map.  It will
not be effective if the map already exists."
  (declare (indent 1))
  (let ((options nil)
        (bindings args))
    (while (keywordp (car bindings))
      (push (pop bindings) options)
      (push (pop bindings) options))
    `((defvar-keymap ,keymap
        ,@(nreverse options)
        ,@(mapcan (lambda (binding) (list (car binding) (cdr binding))) bindings)))))

(provide 'pewcfg--keyword-map)
;;; pewcfg--keyword-map.el ends here
