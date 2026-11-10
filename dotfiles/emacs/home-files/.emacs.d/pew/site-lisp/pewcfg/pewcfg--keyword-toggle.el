;;; pewcfg--keyword-toggle.el --- :toggle -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:toggle (forms)
  "Normalize function for ':toggle'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:toggle (variable &rest values)
  "Create an interactive command to toggle variable from a list of values.
VARIABLE is a symbol of the variable.
VALUES is a list of values that the VARIABLE can be possibly set to.
If VALUES is nil, the VARIABLE will be toggled between (t nil) by default.
A new command `pew-toggle-VARIABLE' and variable `pew-toggle-VARIABLE' will be created.
The variable is used for multiple purposes.  The car of the variable stores the
current index of the list of values that is stored in the cdr."
  (declare (indent 0))
  (let ((toggle-symbol (intern (format "pew-toggle-%s" variable))))
    `((defvar ,toggle-symbol ,(if values
                                   `(list -1 ,@values)
                                 '(list -1 t))
        ,(format  "A list of values used by `%s' command.
The first element is the index which points to the current value.  The index
cycles through the list each the toggle command is called." toggle-symbol))
      (defun ,toggle-symbol ()
        ,(format "toggle the value of variable `%s'.
The values are read from the list `%s'." variable toggle-symbol)
        (interactive)
        (setq ,variable (nth (setcar ,toggle-symbol
                                     (mod (1+ (car ,toggle-symbol))
                                          (length (cdr ,toggle-symbol))))
                             (cdr ,toggle-symbol)))
        (message "Set %s: %S" ',variable ,variable)))))

(provide 'pewcfg--keyword-toggle)
;;; pewcfg--keyword-toggle.el ends here
