;;; pack-typing.el --- Coding convenience -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;==============================================================================
;; Syntax checker
;;==============================================================================

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode 1))

;;==============================================================================
;; Code completion
;;==============================================================================

;; Company core package
(use-package company
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-c i" . company-complete)
         ([remap completion-at-point] . company-complete)
         ([remap indent-for-tab-command] . company-indent-or-complete-common)
         :map company-active-map
         ("<tab>" . company-complete)
         ("<return>" . company-abort)
         ("RET" . company-abort)
         ("C-SPC" . company-search-abort)
         ("C-d" . company-show-doc-buffer)
         ("C-f" . company-show-location))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-tooltip-idle-delay 0.5
        company-idle-delay 0.2
        company-show-numbers t
        company-show-quick-access t
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-auto-complete nil)
  (global-company-mode 1)
  :config
  (company-tng-mode -1))

;; Company improvement
(defun pew/company-box/doc-toggle (&optional status)
  "Toggle company box doc display if STATUS is omitted.
Otherwise if STATUS is given, the status of doc display depends on the value
of STATUS.  Possible values are:
  'show: Display doc.
  'hide: Do not display doc.
  Other values: Toggle doc display status."
  (interactive)
  (cond ((eq 'show status)
         (setq company-box-doc-enable t))
        ((eq 'hide status)
         (setq company-box-doc-enable nil))
        (t (setq company-box-doc-enable (not company-box-doc-enable)))))

(defun pew/company-box/on-start ()
  "Company-box on-start actions."
  (pew/company-box/doc-toggle 'hide))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
         ("C-k" . company-box-doc-manually))
  :init
  (setq company-box-doc-enable nil
        company-box-doc-delay 0.5
        company-box-enable-icon t
        company-box-color-icon t
        company-box-show-single-candidate 'always
        company-box-scrollbar t))

;;==============================================================================
;; Snippets
;;==============================================================================

(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  ;;:config
  ;;(yas-global-mode 1)
  )

(provide 'pack-typing)
;;; pack-typing.el ends here
