;;; pewcfg--keyword-transient.el --- :transient -*- lexical-binding: t; -*-
;;; Commentary:

;; This must be loaded after `pewcfg--keyword-map.el'.

;;; Code:

(defun pewcfg--normalize-:transient (forms)
  "Normalize function for ':transient'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:transient (command &rest bindings)
  "Create an interactive COMMAND that enters transient mode when invoked.
COMMAND is a symbol of the command.
BINDINGS is the same with `pewcfg--generate-:bind'.
A map 'COMMAND-map' and interactive commands 'COMMAND' and 'COMMAND-repeat' will
be created.  See `set-transient-map'.
NOTE: \\`C-g' and \\`C-h' should be preserved.
NOTE: Discouraged `repeat-map' property method in Emacs 28 since it require some
extra work and potentially decrease startup speed.  It needs `repeat-mode' to be
enabled and put the following code for the keymap.
  (map-keymap (lambda (key cmd) (put cmd 'repeat-map 'keymap) keymap)"
  (declare (indent 1))
  (let ((command-map (intern (format "%s-map" command)))
        (command-repeat (intern (format "%s-repeat" command))))
    ;; Override "C-g"
    `(,@(apply 'pewcfg--generate-:map command-map (nconc bindings '(("C-g" . #'keyboard-quit))))
      (defun ,command (arg)
        ,(format "Activate map `%s' temporarily.
If prefix ARG is given the map will be activated in a repeatable manner." command-map)
        (interactive "P")
        (if arg
            (set-transient-map ,command-map
                               (lambda () (not (equal (this-command-keys) (kbd "C-g"))))
                               nil
                               ,(format "%s activated, C-g to exit" command-repeat))
          (set-transient-map ,command-map nil nil ,(format "%s activated" command))))
      (defun ,command-repeat ()
        ,(format "Activate map `%s' in a repeatable manner." command-map)
        (interactive)
        (,command :repeat)))))

(provide 'pewcfg--keyword-transient)
;;; pewcfg--keyword-transient.el ends here
