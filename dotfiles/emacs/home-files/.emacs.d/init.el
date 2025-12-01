;;; init.el --- pew bootstraps -*- lexical-binding: t; coding: utf-8; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Emacs version check
(let ((min-ver "29"))
  (if (version< emacs-version min-ver)
      (error "[pew] Emacs version %s+ is required" min-ver)))

;;; Path setup
(let ((default-directory (file-name-directory load-file-name))) ;; Directory where this init.el resides
  ;; Pew config home
  (defvar pew-toplevel-dir default-directory)
  ;; Configurations from the interactive `customize' interfaces.
  (setq custom-file (expand-file-name "custom.el"))
  ;; All Pew modules
  (setq load-path (nconc (list (expand-file-name "pew/lisp")
                               (expand-file-name "pew/site-lisp"))
                         load-path))
  (let ((default-directory (expand-file-name "pew/site-lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

;;; Module loading
;; NOTE: The loading sequence is important
(require 'pewcfg)
(require 'pewlib)
(require 'init-boot)
(require 'init-package)
(require 'init-config)

;; Load ELPA packages
(require 'elpa-ui)
(require 'elpa-evil)
(require 'elpa-completion-company)
;; (require 'elpa-completion-corfu)
;; (require 'elpa-minibuffer-ivy)
(require 'elpa-minibuffer-vertico)
(require 'elpa-utils)
(require 'elpa-org)
(require 'elpa-copilot)

;; Language supports
(require 'elpa-langs)
(require 'elpa-treesit)
;; (require 'elpa-lsp) ;; LSP is the dependency of language modules
(require 'elpa-eglot)

;; Load custom configuration which takes the highest precedence
(load custom-file :noerror)
(message "[pew] Normal init finished")

(provide 'init)
;;; init.el ends here
