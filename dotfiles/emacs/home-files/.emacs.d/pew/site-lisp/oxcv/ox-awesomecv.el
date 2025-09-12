;; -*- lexical-binding: t; -*-

(require 'org)
(require 'ox-latex)
(require 'subr-x)
(require 'oxcv)

;; Backend definition

(org-export-define-derived-backend 'ox-awesomecv 'latex
  :options-alist
  oxcv-backend-options-alist

  :menu-entry
  `(?f "Export My Resume" ((?a "AwesomeCV File" ,(oxcv-export-to-file 'ox-awesomecv))
                           (?A "AwesomeCV PDF" ,(oxcv-export-to-pdf 'ox-awesomecv "xelatex %s"))))

  :translate-alist
  '((headline . oxa--transcode-headline)
    (plain-list . oxa--transcode-plain-list)
    (item . oxa--transcode-item)
    (template . oxa--transcode-template)))

;; Transcoders

(defun oxa--transcode-template (contents info)
  (string-join
   (list
    "% --- Preamble --"
    ""
    (with-temp-buffer
      (insert-file-contents (plist-get info :cv_preamble))
      (buffer-string))
    ""
    ""
    "% --- Information ---"
    ""
    (let ((name (split-string (plist-get info :cv_author) " ")))
      (format "\\name{%s}{%s}" (car name) (cadr name)))
    (format "\\position{%s}" (plist-get info :cv_title))
    (format "\\mobile{%s}" (plist-get info :cv_mobile))
    (format "\\email{%s}" (plist-get info :cv_email))
    (format "\\linkedin{%s}" (plist-get info :cv_linkedin))
    (format "\\github{%s}" (plist-get info :cv_github))
    ""
    "\\begin{document}"
    ""
    "\\makecvheader[C]"
    (format "\\makecvfooter{%s}{%s}{%s}"
            (plist-get info :cv_footer_left)
            (plist-get info :cv_footer_middle)
            (plist-get info :cv_footer_right))
    ""
    ""
    "% --- Contents ---"
    ""
    contents
    ""
    "\\end{document}")
   "\n"))

(defun oxa--transcode-headline (headline contents info)
  (cond ((oxcv-headline-pagebreak-p headline)
         (oxa--format-pagebreak headline contents info))
        ((oxcv-headline-section-p headline)
         (oxa--transcode-section-headline headline contents info))
        (t
         (oxa--transcode-common-headline headline contents info))))

(defun oxa--transcode-section-headline (headline contents info)
  (let* ((contents (if (not (org-element-property :CV_HEADLINE_TYPE headline))
                       contents ;; Output as is when CV_HEADLINE_TYPE is not presented
                     (oxa--format-headline headline contents info))))
    (pcase (org-element-property :level headline)
      (1 (oxa--format-cvsection headline contents info))
      (2 (oxa--format-cvsubsection headline contents info))
      (_ ""))))

(defun oxa--transcode-common-headline (headline contents info)
  (oxa--format-headline headline contents info))

(defun oxa--transcode-plain-list(plain-list contents info)
  (let ((type (org-element-property :type plain-list)))
    (cond ((eq type 'descriptive)
           (oxa--format-cvskills plain-list contents info))
          (t
           (oxa--format-cvitems plain-list contents info)))))

(defun oxa--transcode-item (item contents info)
  (let ((type (org-element-property :type (org-element-property :parent item))))
    (cond ((eq type 'descriptive)
           (oxa--format-cvskill item contents info))
          (t
           (oxa--format-cvitem item contents info)))))

(defun oxa--format-headline (headline contents info)
  (pcase (org-element-property :CV_HEADLINE_TYPE headline)
    ;; A list of types that can be set in CV_HEADLINE_TYPE property of headlines
    ("cventries" (oxa--format-cventries headline contents info))
    ("cvsubentries" (oxa--format-cvsubentries headline contents info))
    ("cvhonors" (oxa--format-cvhonors headline contents info))
    ("cvparagraph" (oxa--format-cvparagraph headline contents info))
    ("cventry" (oxa--format-cventry headline contents info))
    ("cvsubentry" (oxa--format-cvsubentry headline contents info))
    ("cvhonor" (oxa--format-cvhonor headline contents info))
    (_ ""))) ;; Discard contents of invalid type

(defun oxa--format-cvsection (headline contents info)
  (format "\n\\cvsection{%s}\n%s\n"
          (org-element-property :raw-value headline)
          contents))

(defun oxa--format-cvsubsection (headline contents info)
  (format "\n\\cvsubsection{%s}\n%s\n"
          (org-element-property :raw-value headline)
          contents))

(defun oxa--format-cventries (headline contents info)
  (format "\n\\begin{cventries}\n\n%s\n\n\\end{cventries}\n"
          (string-trim contents)))

(defun oxa--format-cvsubentries (headline contents info)
  (format "\n\\begin{cvsubentries}\n\n%s\n\n\\end{cvsubentries}\n"
          (string-trim contents)))

(defun oxa--format-cvhonors (headline contents info)
  (format "\n\\begin{cvhonors}\n\n%s\n\n\\end{cvhonors}\n"
          (string-trim contents)))

(defun oxa--format-cvitems (plain-list contents info)
  (format "\n\\begin{cvitems}\n%s\n\\end{cvitems}\n"
          (string-trim (oxcv-compact-newlines contents))))

(defun oxa--format-cvskills (plain-list contents info)
  (format "\n\\begin{cvskills}\n\n%s\n\n\\end{cvskills}\n"
          (string-trim contents)))

(defun oxa--format-cvparagraph (headline contents info)
  (format "\n\\begin{cvparagraph}\n\n%s\n\n\\end{cvparagraph}\n"
          (string-trim contents)))

(defun oxa--format-cventry (headline contents info)
  (format "\n\\cventry\n{%s}\n{%s}\n{%s}\n{%s}\n{%s}\n"
          (org-element-property :raw-value headline)
          (org-element-property :CV_ENTRY_ORGANIZATION headline)
          (org-element-property :CV_ENTRY_LOCATION headline)
          (org-element-property :CV_ENTRY_TIME headline)
          contents))

(defun oxa--format-cvsubentry (headline contents info)
  (format "\n\\cvsubentry\n{}\n{%s}\n{%s}\n{}\n"
          ""
          (org-element-property :raw-value headline)
          (org-element-property :CV_SUBENTRY_TIME headline)
          ""))

(defun oxa--format-cvhonor (headline contents info)
  (format "\n\\cvhonor\n{%s}\n{%s}\n{%s}\n{%s}\n"
          (org-element-property :raw-value headline)
          (org-element-property :CV_HONOR_DESCRIPTION headline)
          (org-element-property :CV_HONOR_LOCATION headline)
          (org-element-property :CV_HONOR_TIME headline)))

(defun oxa--format-cvitem (item contents info)
  (format "\n\\item{%s}\n" (string-trim contents)))

(defun oxa--format-cvskill (item contents info)
  (let ((tag (org-element-property :tag item)))
    (format "\n\\cvskill\n{%s}\n{%s}\n"
            (org-export-data tag info)
            (string-trim (or contents "")))))

(defun oxa--format-pagebreak (headline contents info)
  "\n\\clearpage\n")


(provide 'ox-awesomecv)

;; Local Variables:
;; read-symbol-shorthands: (("oxa-" . "ox-awesomecv-"))
;; End:
