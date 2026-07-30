;;; elpa-magit.el --- Git frontend -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'seq)

(use-package magit
  :straight t
  :commands magit-status

  :bind
  (("C-x v"   . magit-status)
   ("C-x C-v" . magit-file-dispatch))

  :custom
  ;; Don't use the default bindings under "C-x" prefix
  (magit-define-global-key-bindings nil)

  :preface
  (defvar pew-magit-simple-hook-mask-alist
    '((magit-refs-sections-hook . (magit-insert-remote-branches
                                   magit-insert-tags)))
    "Prevents certain hook from running when `pew-magit-simple-mode' is active.

The alist is of the form (HOOK-SYMBOL . MASKS).

MASKS is a list of hook functions that HOOK-SYMBOL potentially invokes. They will
be removed from the execution list once `pew-magit-simple-mode' is active.")

  (defvar pew-magit-simple--saved-hooks nil
    "An alist to store the original hook definitions.

When `pew-magit-simple-mode' is activated, it checks hooks defined in
`pew-magit-simple-hook-mask-alist' and stores their values in this variable if
not stored yet. When `pew-magit-simple-mode' is deactivated, it restores values
from this variable to corresponding hooks, if saved any.")

  (define-minor-mode pew-magit-simple-mode
    "A minor mode to turn off some magit hooks to improve performance."
    :lighter "pew-magit-simple"
    :global t
    (cond (pew-magit-simple-mode
           (pcase-dolist (`(,hook-symbol . ,masks) pew-magit-simple-hook-mask-alist)
             ;; Save original hook definitions if not saved yet
             (let ((hook-value (symbol-value hook-symbol)))
               (unless (assq hook-symbol pew-magit-simple--saved-hooks)
                 (setq pew-magit-simple--saved-hooks
                       (cons (cons hook-symbol hook-value)
                             pew-magit-simple--saved-hooks)))
               (setf (symbol-value hook-symbol)
                     (seq-reduce (lambda (acc m) (remq m acc))
                                 masks
                                 hook-value)))))
          (t
           (pcase-dolist (`(,hook-symbol . ,masks) pew-magit-simple-hook-mask-alist)
             (let ((saved (assq hook-symbol pew-magit-simple--saved-hooks)))
               (when saved
                   (setf (symbol-value hook-symbol) (cdr saved)))))))))

(provide 'elpa-magit)
;;; elpa-magit.el ends here
