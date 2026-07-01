;;; prelude.el --- a tiny macro-expansion test framework -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:

;;; Code:

(require 'ert)
(require 'pcase)

(defun macro-test--expand (form &optional full)
  "Expand FORM once, or fully if FULL is non-nil."
  (if full
      (macroexpand-all form)
    (macroexpand-1 form)))

(defmacro match--pattern (expr pattern)
  "A simple wrapper matches EXPR with PATTERN."
  `(pcase ,expr
     (,pattern t)
     (_ nil)))

(defmacro should-match (expr pattern)
  `(progn
     (unless (match--pattern ,expr ,pattern)
       (ert-fail
        (list "Expression did not match pattern"
              :expression ',expr
              :pattern    ',pattern
              :actual     (eval ',expr))))
     (should t)))

(defmacro should-expand-to (form pattern &optional full)
  "Assert FORM's macro expansion matches PATTERN.
PATTERN is a `pcase' pattern (typically a backquote pattern).
If FULL is non-nil, expand fully with `macroexpand-all'; otherwise
one step with `macroexpand-1'.  FORM is not evaluated."
  `(let ((actual (macro-test--expand ',form ,full)))
     (unless (pcase actual
               (,pattern t)
               (_ nil))
       (ert-fail
        (list "Macro expansion did not match pattern"
              :form    ',form
              :pattern ',pattern
              :actual  actual)))
     ;; Also register a `should' so ERT counts the assertion:
     (should t)))

(defmacro should-fully-expand-to (form pattern)
  "Like `should-expand-to' but fully expands FORM."
  `(should-expand-to ,form ,pattern t))

(provide 'prelude)
;;; prelude.el ends here
