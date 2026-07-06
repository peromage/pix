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
;; --- Bootstrap ---
(require 'pewcfg)
(require 'pewlib)
(require 'init-boot)
(require 'init-package)
(require 'init-config)

;; --- Basics ---
;; (require 'elpa-minibuffer-ivy)
(require 'elpa-minibuffer-vertico)
;; (require 'elpa-completion-corfu)
(require 'elpa-completion-company)

;; --- Languages ---
;; (require 'elpa-lsp)
(require 'elpa-eglot)
(require 'elpa-treesit)
(require 'elpa-copilot)
(require 'elpa-claude-code-ide)
;; (require 'elpa-flycheck)
(require 'elpa-langs)
(require 'elpa-lang-cpp)
(require 'elpa-lang-python)
(require 'elpa-lang-nix)
(require 'elpa-lang-lua)
(require 'elpa-lang-cmake)
(require 'elpa-lang-kdl)
;; (require 'elpa-lang-plantuml)
;; (require 'elpa-lang-graphviz-dot)
(require 'elpa-lang-mermaid)

;; --- Daily drivers ---
(require 'elpa-magit)
(require 'elpa-git-gutter)
;; (require 'elpa-vterm)
(require 'elpa-ghostel)
(require 'elpa-paredit)
(require 'elpa-separedit)
(require 'elpa-ace-window)
(require 'elpa-avy)
(require 'elpa-yasnippet)
(require 'elpa-evil)
(require 'elpa-org)

;; --- Misc ---
(require 'elpa-utils)
(require 'elpa-keycast)
;; (require 'elpa-which-key)
;; (require 'elpa-treemacs)

;; --- Appearance ---
;; (require 'elpa-ui-doom-modeline)
(require 'elpa-ui-fonts)
(require 'elpa-ui-prot-modeline)
(require 'elpa-ui-themes)

;; ANCHOR-PRE-CUSTOM-EL

;; Load custom configuration which takes the highest precedence
(load custom-file :noerror)

;; ANCHOR-POST-CUSTOM-EL

(message "[pew] init.el finished")

(provide 'init)
;;; init.el ends here
