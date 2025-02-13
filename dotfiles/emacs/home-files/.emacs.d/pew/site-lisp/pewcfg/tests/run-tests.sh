#!/usr/bin/env -S emacs --batch --script
;;; run-tests.sh --- unit tests entry -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

(require 'files)

(unless (and (file-exists-p "pewcfg.el")
             (file-exists-p "pewcfg-core.el")
             (file-exists-p "pewcfg-use-package.el"))
  (error "Working directory must be pewcfg root!"))

;;; Load paths
(add-to-list 'load-path default-directory)
(normal-top-level-add-subdirs-to-load-path)

;;; Load required modules
(require 'pewcfg)
(require 'common-test-defs)
(require 'test-pewcfg-core)
(require 'test-pewcfg-use-package)

(kill-emacs (execute-test-suites 'execute-suite-test-pewcfg-core
                                 'execute-suite-test-pewcfg-use-package))
