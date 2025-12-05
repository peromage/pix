;;; elpa-utils.el --- supplementary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Lazy loading for these packages

;; Search
(use-package rg :defer t)
;; Focused view
(use-package olivetti :defer t)
;; Cats!!!
(use-package nyan-mode :defer t)
(use-package zone-nyan :defer t)
;; Colorful parenthesises
(use-package rainbow-delimiters :defer t)
;; Colorize color code
(use-package rainbow-mode :defer t)
;; Highlight current line
(use-package beacon :defer t)
;; Hyperbole
(use-package hyperbole :defer t)
;; Useful commands
(use-package crux :defer t)

;;; Editing

;; Plan B.  In case `flymake' doesn't have checkers for certain languages
(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode))

;; TODO: Maybe use tempel?
(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "pew/yasnippet" pew-toplevel-dir)))
  (yas-indent-line 'fixed)
  :config
  (yas-global-mode 1))

(use-package separedit
  :bind ( :map pew-M-u-map
          ("'" . separedit-dwim)) )

(use-package paredit
  :hook ((lisp-interaction-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)))

(use-package avy
  :bind ( :map pew-M-u-map
          ("f"   . avy-goto-char)
          ("M-f" . avy-goto-line)) )

(use-package ace-window
  :bind ( :map pew-M-u-map
          ("w" . ace-window)
          ("W" . ace-swap-window)) )

;;; Git

(use-package magit
  :commands magit-status
  :bind ( :map pew-M-u-map
          ("g"   . magit-status)
          ("M-g" . magit-file-dispatch) )
  :custom
  ;; Don't use the default bindings under "C-x" prefix
  (magit-define-global-key-bindings nil))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign "**")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  (git-gutter:unchanged-sign nil)
  (git-gutter:separator-sign nil)
  (git-gutter:update-interval 2)
  (git-gutter:visual-line nil)
  (git-gutter:hide-gutter nil)
  (git-gutter:verbosity 0)

  :config
  (global-git-gutter-mode 1)

  (pewcfg
    :face
    (git-gutter:modified   :foreground  "yellow"       :background  "unspecified")
    (git-gutter:added      :foreground  "green"        :background  "unspecified")
    (git-gutter:deleted    :foreground  "red"          :background  "unspecified")
    (git-gutter:unchanged  :foreground  "unspecified"  :background  "unspecified")
    (git-gutter:separator  :foreground  "unspecified"  :background  "unspecified")))

;;; Panels

(use-package vterm
  :if (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
  :commands (vterm vterm-other-window)
  :hook (vterm-mode . pewlib-terminal-mode-setup)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-keymap-exceptions '("C-z" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x"
                             "M-o" "C-y" "M-y"))
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("plink" "/bin/bash")))

  :preface
  (defun pew-vterm-new (shell)
    "Create a new vterm window.
ARG is a prefix argument.  If it is non-nill, a prompt will pop up to allow
users to specify the shell to start with."
    (interactive "sShell: ")
    (unless (boundp 'vterm-shell)
      (defvar vterm-shell))
    (let ((vterm-shell shell))
      (vterm :new))))

(use-package treemacs
  :commands treemacs
  :hook (treemacs-mode . pew-treemacs-mode-setup)
  :bind ( :map treemacs-mode-map
          ("j" . treemacs-find-file) )
  :custom
  (treemacs-wrap-around nil)
  (treemacs-eldoc-display 'detailed)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory nil)

  :preface
  (defun pew-treemacs-mode-setup ()
    "`treemacs-mode' initialization."
    (display-line-numbers-mode -1))

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;;; Interactive

(use-package keycast
  :commands (keycast-log-mode
             keycast-tab-bar-mode
             keycast-header-line-mode
             keycast-mode-line-mode)
  :custom
  (keycast-mode-line-format "%2s%k%c%R")
  (keycast-mode-line-window-predicate 'mode-line-window-selected-p) ;; Show in current window
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-tab-bar-format "%k%c%R")
  (keycast-header-line-format "%k%c%R")
  (keycast-header-line-remove-tail-elements nil)
  :config
  (setq keycast-substitute-alist (nconc '((self-insert-command "." "Typing...")
                                          (mouse-event-p nil)
                                          (mouse-movement-p nil)
                                          (mouse-set-point nil)
                                          (mouse-drag-region nil)
                                          (mwheel-scroll nil)
                                          (handle-select-window nil))
                                        keycast-substitute-alist)))

(use-package which-key
  :commands which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode 1)
  ;; Minibuffer usually causes display problems
  ;;(which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

(provide 'elpa-utils)
;;; elpa-utils.el ends here
