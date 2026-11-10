;;; pewcfg--keyword-vcpkgs.el --- :vcpkgs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun pewcfg--normalize-:vcpkg (forms)
  "Normalize function for ':vcpkg'."
  (mapcar #'pewcfg-normalize-identity forms))

(defun pewcfg--generate-:vcpkg (name package rev &optional backend)
  "Install package from a VC source.
This is a wrapper of `package-vc-install'.  See that function for parameter
explanation.

NOTE: The package can't be upgraded along with normal package update.  The old
package must be deleted before installing a new version, assuming the REV is not
pinned to a specific revision.

See: https://tony-zorman.com/posts/package-vc-install.html"
  `((unless (package-installed-p ',name)
      (package-vc-install ,package ,rev ,backend ',name))))

(provide 'pewcfg--keyword-vcpkgs)
;;; pewcfg--keyword-vcpkgs.el ends here
