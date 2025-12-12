;;; elpa-org.el --- org mode -*- lexical-binding: t; -*-
;;; Commentary:

;; For the babel reference: https://org-babel.readthedocs.io/en/latest/

;;; Code:

(use-package org
  :commands org-mode
  :hook (org-babel-after-execute . pew-org-refresh-images)

  :custom
  ;; Visual on startup
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-startup-indented t)
  (org-startup-folded 'nofold)
  (org-startup-truncated nil)
  (org-startup-numerated nil)
  (org-startup-with-inline-images nil)
  (org-hide-block-startup nil)
  ;; Default marker visibility
  (org-ellipsis " ...")
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)
  (org-hide-macro-markers nil)
  (org-link-descriptive nil)
  (org-pretty-entities nil)

  ;; Image displaying
  (org-display-remote-inline-images 'skip)

  ;; Fontify
  (org-src-fontify-natively t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-emphasized-text t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)

  ;; Editing
  (org-return-follows-link nil)
  (org-insert-heading-respect-content t)
  (org-fold-catch-invisible-edits 'smart)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  (org-use-fast-tag-selection nil) ;; Always use list selection
  (org-src-preserve-indentation t)
  (org-refile-targets '((nil :maxlevel . 10)))

  ;; Indentation
  ;; No hard indentation: https://orgmode.org/manual/Hard-indentation.html
  (org-odd-levels-only nil)
  (org-adapt-indentation nil)

  ;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Clock
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks nil)
  (org-clock-clocked-in-display 'mode-line)
  (org-clock-ask-before-exiting t)
  (org-clock-rounding-minutes 0) ;; Keep it precise
  (org-clock-out-when-done t)
  (org-clock-persist nil)

  ;; Refile
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Babel
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t)
                              (perl . t)))
  (org-confirm-babel-evaluate nil)

  ;; Todo
  (org-use-fast-todo-selection 'expert) ;; No popup window
  ;; Omit selection characters after the first general sequence to let Org
  ;; generate them automatically
  (org-todo-keywords (pewlib-load-data-file (expand-file-name "pew/org-templates/todo.eld" pew-toplevel-dir)))
  (org-enforce-todo-dependencies nil)
  (org-enforce-todo-checkbox-dependencies nil)

  ;; Templates
  (org-capture-templates (pewlib-load-data-file (expand-file-name "pew/org-templates/capture.eld" pew-toplevel-dir)))

  :preface
  (define-minor-mode pew-org-show-marker-mode
    "Minor mode to toggle marker visibility."
    :lighter nil
    (when (not (local-variable-if-set-p 'org-hide-emphasis-markers))
      (make-variable-buffer-local 'org-hide-emphasis-markers)
      (make-variable-buffer-local 'org-hide-leading-stars)
      (make-variable-buffer-local 'org-hide-macro-markers)
      (make-variable-buffer-local 'org-link-descriptive)
      (make-variable-buffer-local 'org-pretty-entities))
    (cond (pew-org-show-marker-mode
           (setq-local org-hide-emphasis-markers t)
           (setq-local org-hide-leading-stars t)
           (setq-local org-hide-macro-markers t)
           (setq-local org-link-descriptive t)
           (setq-local org-pretty-entities t))
          (t
           (setq-local org-hide-emphasis-markers nil)
           (setq-local org-hide-leading-stars nil)
           (setq-local org-hide-macro-markers nil)
           (setq-local org-link-descriptive nil)
           (setq-local org-pretty-entities nil)))
    (org-mode-restart))

  (defun pew-org-refresh-images ()
    "Redisplay inline images if they exist in the current buffer."
    (interactive)
    (if org-inline-image-overlays
        (org-redisplay-inline-images)))

  (defun pew-org-add-src-lang-modes (alist)
    "Add modes defined in ALIST to `org-src-lang-modes'.
Duplicated pairs will be removed."
    (mapc (lambda (x) (assoc-delete-all (car x) org-src-lang-modes)) alist)
    (setq org-src-lang-modes (nconc alist org-src-lang-modes)))

  (defun pew-org-add-babel-load-languages (alist)
    "Add languages defined in ALIST to `org-babel-load-languages'.
`org-babel-do-load-languages' will be called underneath."
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (nconc alist org-babel-load-languages)))

  (defun pew-org-set-directory (dir)
    "Update `org-directory' and related paths with given DIR."
    (setq org-directory dir)
    (setq org-default-notes-file (expand-file-name "default.org" org-directory))
    (setq org-agenda-files (list (expand-file-name "agenda.org" org-directory))))

  (defun pew-org-goto-heading (level &optional to-end)
    "Move cursor to the selected heading in the current `org-mode' buffer.
Minibuffer will show up with the specified LEVEL of headings and move cursor to
it once the choice is confirmed.
Level can be the following values:
  - 0: All the headings are selectable.
  - nil: Same with 0.
  - non-zero number: Filter this level of headings.
  - non-nil object: Interactively enter level number.
If TO-END is non-nil the cursor will be moved to the end of the heading.
Otherwise the cursor is placed at the beginning of the heading."
    (interactive "P")
    (if (not (eq 'org-mode major-mode))
        (message "Not an org buffer.")
      (setq level (cond ((null level) 0)
                        ((not (numberp level)) (read-number "Search heading level: "))
                        (t level)))
      (let* ((headings (mapcar (lambda (e) (cons (org-element-property :title e) e))
                               (seq-filter
                                (if (zerop level) #'identity
                                  (lambda (e) (= level (org-element-property :level e))))
                                (org-map-entries #'org-element-at-point))))
             (selected (cdr (assoc
                             (completing-read "Select a heading: " headings nil t)
                             headings))))
        ;; When used in `org-capture-templateas', `narrow-to-region' can be used
        ;; together with `:unnarrowed' to resume from existing entries.
        (goto-char (org-element-property
                    (if to-end :end :begin)
                    selected)))))

  (defun pew-org-find-file ()
    "Find files under `org-directory'."
    (interactive)
    (let ((default-directory (file-name-as-directory org-directory)))
      (call-interactively #'find-file)))

  :config
  ;; Refer to: https://org-babel.readthedocs.io/en/latest/header-args/
  (setq org-babel-default-header-args '((:session . "none")
                                        (:results . "output replace")
                                        ;; (:wrap . "example") ;; Might be problematic for pictures
                                        (:exports . "both")
                                        (:eval . "never-export")
                                        (:cache . "no")
                                        (:noweb . "yes")
                                        (:hlines . "no")
                                        (:tangle . "no")))
  (setq org-babel-default-inline-header-args '((:session . "none")
                                               (:results . "output replace")
                                               (:exports . "results")
                                               (:eval . "never-export")
                                               (:cache . "no")
                                               (:noweb . "yes")
                                               (:hlines . "no")
                                               (:tangle . "no")))

  ;; Agenda and capture
  (pew-org-set-directory (expand-file-name "orgfiles" pew-toplevel-dir))) ;; End org


(use-package org-bullets
  :hook (org-mode . pew-org-bullets-setup)

  :preface
  (defun pew-org-bullets-setup ()
    "`org-bullets' initialization."
    (org-bullets-mode 1)))

;; Org contrib {

(use-package org-contrib :after org)
;; Included in org-contrib
(use-package ox-extra :straight nil :after org)
(use-package org-tempo :straight nil :after org)

;; } Org contrib

;; Org to Markdown for Hugo
(use-package ox-hugo :after org)
;; Export backend for GitHub flavored Markdown
(use-package ox-gfm :after org)
;; My own backends
(use-package ox-awesomecv :straight nil :after org)

(provide 'elpa-org)
;;; elpa-org.el ends here
