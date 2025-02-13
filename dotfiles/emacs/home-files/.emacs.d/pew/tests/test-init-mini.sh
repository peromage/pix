#!/usr/bin/env -S emacs --batch --script
;;; test-init-mini.sh --- Test minimal startup -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(unless (and (file-directory-p "pew") (file-exists-p "init-mini.el"))
  (error "Working directory must be Pew config root!"))

;;; Test starts
(require 'url-vars)
(let* ((debug-on-error t)
       (url-show-status nil)
       (user-emacs-directory default-directory)
       (user-init-file (expand-file-name "init-mini.el" user-emacs-directory))
       (load-path (delq user-emacs-directory load-path)))
  (load-file user-init-file)
  (run-hooks 'after-init-hook)
  (run-hooks 'emacs-startup-hook)
  (message "%s started in %s" (emacs-version) (emacs-init-time)))
