;;; pewcfg-core.el --- pewcfg core components -*- lexical-binding: t; -*-

;; Author:  Fang Deng <fang@elfang.com>

;;; Commentary:

;; How to add new keyword supports:
;; 1. Define a function named 'pewcfg--normalize-:KEYWORD', which takes a list
;; of forms specified after the keyword in `pewcfg', and returns a list of forms.
;; Each form in the list contains arguments for the generate function in the
;; next step.
;; 2. Define a function named 'pewcfg--generate-:KEYWORD', which takes arguments
;; from the previous step, and returns a list of forms that can be evaluated at
;; execution time.

;;; Code:

(require 'subr-x)

;;; Variable definitions
(defvar pewcfg-keywords '(:custom
                          :customize
                          :setq
                          :setq-default
                          :bind
                          :map
                          :transient
                          :toggle
                          :face
                          :property
                          :hook
                          :automode
                          :eval
                          :eval-after
                          :vcpkg)
  "An alist of keywords used by `pewcfg' to specify sections.

Each keyword must implement two functions named 'pewcfg--normalize-:keyword' and
'pewcfg--generate-:keyword'.

Each form after the keyword is collected into a list and passed to the normalize
function. The normalize function takes that list of forms and transform it into
another list, where each element is a list of arguments passing to the generate
function (matching its signature). The generate function then returns a list of
forms to be evaluated.

List of each keyword's form signature:
  :custom       (VARIABLE VALUE [COMMENT])
  :customize    (VARIABLE VALUE [COMMENT])*
  :setq         (VARIABLE VALUE [COMMENT]) ;; COMMENT has no effect
  :setq-default (VARIABLE VALUE [COMMENT]) ;; COMMENT has no effect
  :bind         (KEYMAP [(KEY . DEFINITION) ...])
  :map          (KEYMAP [:KEYWORD VALUE ...] [(KEY . DEFINITION) ...])
  :transient    (COMMAND [:KEYWORD VALUE ...] [(KEY . DEFINITION) ...])
  :toggle       (VARIABLE [VALUE VALUE ...])
  :face         (FACE [:KEYWORD VALUE ...])
  :property     (SYMBOL [(PROPERTY . VALUE) ...])
  :hook         (NAME FUNCTION [DEPTH])
  :automode     (MATCHER MODE [FALLTHROUGH])
  :eval         (SEXP)
  :eval-after   (FEATURE BODY)
  :vcpkg        (NAME PACKAGE REV [BACKEND])

* The difference between ':custom' and ':customize' is that, ':custom' uses a
synthetic theme `pewcfg-custom-theme' to bind variable values to with the
implementation of `custom-theme-set-variables' , while ':customize' is
implemented with `customize-set-variable' to set variables directly.
Both methods are very similar.  They are all capable of keeping the
`custom-file' clean.  ':custom' may be slightly faster than ':customize'.")

(defvar pewcfg-keyword-normalize-function-format "pewcfg--normalize-%s"
  "The keyword normalize function format.
A normalize functions should take a list of forms and make each of them in a
format that can be applied to the generate function by `apply'.")

(defvar pewcfg-keyword-generate-function-format "pewcfg--generate-%s"
  "The keyword generate function format.
Each generate function can have different signatures but it should always return
a list of forms.")

;;; Form normalization functions
(defun pewcfg-normalize-identity (form)
  "Return FORM unchanged like `identity'."
  form)

(defun pewcfg-normalize-pair (form)
  "Assume that FORM is a cons cell and convert it to a list."
  (list (car form) (cdr form)))

(defun pewcfg-normalize-first-two (form)
  "Assume that FORM is a list with at least two elements and convert it to a list."
  (list (car form) (cadr form)))

(defun pewcfg-normalize-single (form)
  "Wrap a FORM in a list."
  (list form))

;;; List manipulation functions
(defun pewcfg-until-next-keyword (lst)
  "Return a list that starts with the nearest keyword from the head."
  (named-let pewcfg---until-next-keyword ((rest lst))
    (cond ((keywordp (car rest))
           rest)
          ((null rest)
           nil)
          (t
           (pewcfg---until-next-keyword (cdr rest))))))

(defun pewcfg-slice-keyword-segments (lst)
  "Return an alist of keyword segments.
Each cons of the returned alist has the keyword as its car and a list of
elements before the next keyword as its cdr."
  (let* ((start (pewcfg-until-next-keyword lst))
         (next (pewcfg-until-next-keyword (cdr start)))
         (result nil))
    (while start
      (push (butlast start (length next)) result)
      (setq start next)
      (setq next (pewcfg-until-next-keyword (cdr next))))
    (nreverse result)))

;;; Helper functions
(defun pewcfg-tokey (key)
  "Convert KEY to the representation that can be recognized as a keycord.
Possible value could be a string which will be converted with (kbd key).  If KEY
is a vector then does nothing."
  (if (stringp key) (kbd key) key))

;;; Application functions
(defun pewcfg-apply-keyword (keyword &rest forms)
  "Apply FORMS with KEYWORD's handler functions.
The FORMS will be processed by the corresponded normalize function first and
then each element of the results will be expanded as the generate function's
input.
The results of this function is a list of unevaluated forms."
  (if (memq keyword pewcfg-keywords)
      (let ((gfunc (intern-soft (format pewcfg-keyword-generate-function-format keyword)))
            (nfunc (intern-soft (format pewcfg-keyword-normalize-function-format keyword))))
        (mapcan (lambda (form) (apply gfunc form))
                (funcall nfunc forms)))
    (error "Invalid keyword %S" keyword)))

;;; Main entry
(defmacro pewcfg (&rest args)
  "Main entry of pewcfg utilities.
This one is intended to be used instead of calling generate functions directly.
ARGS is a list of forms.  See the registered helpers from `pewcfg-keywords'
for form definitions.
Typical usage is as follow:
  (pewcfg :KEYWORD FORMS :KEYWORD FORMS ...)"
  (declare (indent 0))
  (if (keywordp (car args))
      (cons 'progn (mapcan (lambda (seg)
                             (apply #'pewcfg-apply-keyword seg))
                           (pewcfg-slice-keyword-segments args)))
    (error "Not start with a keyword")))

;;; Load keyword definitions
(require 'pewcfg--keyword-custom)
(require 'pewcfg--keyword-customize)
(require 'pewcfg--keyword-setq)
(require 'pewcfg--keyword-setq-default)
(require 'pewcfg--keyword-bind)
(require 'pewcfg--keyword-map)
(require 'pewcfg--keyword-transient)
(require 'pewcfg--keyword-toggle)
(require 'pewcfg--keyword-face)
(require 'pewcfg--keyword-property)
(require 'pewcfg--keyword-hook)
(require 'pewcfg--keyword-automode)
(require 'pewcfg--keyword-eval)
(require 'pewcfg--keyword-eval-after)
(require 'pewcfg--keyword-vcpkgs)

(provide 'pewcfg-core)
;;; pewcfg-core.el ends here
