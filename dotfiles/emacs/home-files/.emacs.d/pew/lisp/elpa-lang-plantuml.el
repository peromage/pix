;;; elpa-lang-plantuml.el --- PlantUML syntax -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(use-package plantuml-mode
  :straight t
  :mode
  (("\\.puml\\'" . plantuml-mode)
   ("\\.plantuml\\'" . plantuml-mode))

  :custom
  (plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)

  :init
  ;; `org-mode' support
  (pewcfg
    :eval-after
    (org
     (setq org-plantuml-jar-path (locate-user-emacs-file ".cache/plantuml.jar"))
     (setq org-plantuml-exec-mode 'jar)
     (pew-org-add-src-lang-modes '(("plantuml" . plantuml)))
     (pew-org-add-babel-load-languages '((plantuml . t))))))

(provide 'elpa-lang-plantuml)
;;; elpa-lang-plantuml.el ends here
