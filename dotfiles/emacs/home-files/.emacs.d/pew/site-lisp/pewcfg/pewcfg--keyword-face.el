;;; pewcfg--keyword-face.el --- :face -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:face (forms)
  "Normalize function for ':face'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:face (face &rest args)
  "Set FACE attributes.
FACE is a symbol of the face.
ARGS is a plist consists with ATTRIBUTE VALUE pairs.
See `set-face-attribute'."
  (declare (indent 1))
  `((set-face-attribute ',face nil ,@args)))

(provide 'pewcfg--keyword-face)
;;; pewcfg--keyword-face.el ends here
