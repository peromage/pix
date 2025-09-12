;; -*- lexical-binding: t; -*-

(require 'org)
(require 'subr-x)

(defconst oxcv-backend-options-alist '((:cv_title "CV_TITLE" nil "" t)
                                       (:cv_author "CV_AUTHOR" nil "" t)
                                       (:cv_email "CV_EMAIL" nil "" t)
                                       (:cv_mobile "CV_MOBILE" nil "" t)
                                       (:cv_linkedin "CV_LINKEDIN" nil "" t)
                                       (:cv_github "CV_GITHUB" nil "" t)
                                       (:cv_preamble "CV_PREAMBLE" nil "" t)
                                       (:cv_footer_left "CV_FOOTER_LEFT" nil "" t)
                                       (:cv_footer_middle "CV_FOOTER_MIDDLE" nil "" t)
                                       (:cv_footer_right "CV_FOOTER_RIGHT" nil "" t))
  "Option alist used for backend definition.")

(defconst oxcv-headline-property-keywords '(:CV_HEADLINE_TYPE
                                            :CV_ENTRY_ORGANIZATION
                                            :CV_ENTRY_LOCATION
                                            :CV_ENTRY_TIME
                                            :CV_SUBENTRY_TIME
                                            :CV_HONOR_DESCRIPTION
                                            :CV_HONOR_LOCATION
                                            :CV_HONOR_TIME)
  "All supported headline properties.")

(defconst oxcv-headline-environments '("cventries"
                                       "cvsubentries"
                                       "cvhonors"
                                       "cvparagraph")
  "Latex environment types.")

(defconst oxcv-headline-commands '("cventry"
                                   "cvsubentry"
                                   "cvhonor")
  "Latex command types.")

(defun oxcv-headline-environment-p (headline)
  "Return non-nil if HEADLINE has CV_HEADLINE_TYPE property and its value is in `oxcv-headline-environments'."
  (member (org-element-property :CV_HEADLINE_TYPE headline) oxcv-headline-environments))

(defun oxcv-headline-command-p (headline)
  "Return non-nil if HEADLINE has CV_HEADLINE_TYPE property and its value is in `oxcv-headline-commands'."
  (member (org-element-property :CV_HEADLINE_TYPE headline) oxcv-headline-commands))

(defun oxcv-headline-pagebreak-p (headline)
  "Return non-nil if HEADLINE has CV_HEADLINE_TYPE property and its value is pagebreak."
  (equal (org-element-property :CV_HEADLINE_TYPE headline) "pagebreak"))

(defun oxcv-headline-section-p (headline)
  "Return non-nil if HEADLINE is level 1 or level 2."
  (let ((level (org-element-property :level headline)))
    (or (= level 1) (= level 2))))

(defun oxcv-compact-newlines (str)
  "Replace multiple newline characters with only one for all occurrences in STR."
  (replace-regexp-in-string "\n\n+" "\n" str))

(defun oxcv-export-to-file (backend)
  "Return an export lambda that can be used to register menu for BACKEND."
  (lambda (async subtreep visible-only body-only)
    (org-export-to-file backend (org-export-output-file-name ".tex"))))

(defun oxcv-export-to-buffer (backend)
  "Return an export lambda that can be used to register menu for BACKEND."
  (lambda (async subtreep visible-only body-only)
    (org-export-to-buffer backend (org-export-output-file-name "-exported"))))

(defun oxcv-export-to-pdf (backend command)
  "Return an export lambda that can be used to register menu for BACKEND.
The COMMAND must have a literal %s placeholder to indicate where the souce file
is inserted."
  (lambda (async subtreep visible-only body-only)
    (shell-command (format command
                           (funcall (oxcv-export-to-file backend)
                                    async
                                    subtreep
                                    visible-only
                                    body-only)))))


(provide 'oxcv)
