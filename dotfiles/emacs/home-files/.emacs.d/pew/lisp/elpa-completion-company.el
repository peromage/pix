;;; elpa-completion-company.el --- company and complementary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :demand t
  :bind ( :map company-mode-map
          ("C-M-i" . company-complete)
          :map company-active-map
          ("C-c"   . company-complete-selection)
          ("C-k"   . company-abort)
          ;; ("RET"   . nil)
          ;; ("<return>"   . nil)
          :map pew::M-c-map
          ("i"     . company-complete)
          ("f"     . company-files)
          ("t"     . company-gtags)
          ("T"     . company-etags)
          ("d"     . company-dabbrev)
          ("D"     . company-dabbrev-code)
          ("a"     . company-abbrev) )
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  (company-tooltip-idle-delay 0.2)
  (company-tooltip-limit 5)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above nil) ;; Cursor keys are also flipped so keep it off
  (company-show-quick-access 'left)
  (company-show-numbers 'left)
  (company-selection-wrap-around nil)
  (company-insertion-on-trigger nil) ;; Auto commit
  (company-abort-on-unique-match t)
  (company-search-filtering t)
  (company-inhibit-inside-symbols nil)

  :config
  (with-eval-after-load 'orderless
    (define-advice company-capf (:around (capf-fn &rest args) pew::company::completion-style)
      (let ((completion-styles '(basic partial-completion orderless)))
        (apply capf-fn args))))

  (global-company-mode 1)
  (company-tng-mode 1))

(provide 'elpa-completion-company)
;;; elpa-completion-company.el ends here
