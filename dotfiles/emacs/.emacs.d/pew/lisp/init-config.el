;;; init-config.el --- built-in config -*- lexical-binding: t; -*-
;;; Commentary:

;; Set Emacs built-ins with my own flavor.

;;; Code:

(pewcfg
;;; Custom
  :customize
;;;; Startup
  (inhibit-startup-buffer-menu t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message "")

;;;; Windows and frames
  ;; When 3 side windows present `window-toggle-side-windows' may cause problem
  ;; Use `winner-undo' to revert the mess
  (display-buffer-alist `((,(pewlib::workspace::map-buffer-regex '(:shell :terminal) 'concat)
                           ,@(pewlib::workspace::side-window-actions 'bottom 0))
                          (,(pewlib::workspace::map-buffer-regex '(:help :eldoc :man :woman) 'concat)
                           ,@(pewlib::workspace::side-window-actions 'bottom 1))
                          (,(pewlib::workspace::map-buffer-regex '(:message :backtrace :warning :log :compilation :output :command :tree-sitter-explorer :flymake-diagnostics :org-babel) 'concat)
                           ,@(pewlib::workspace::side-window-actions 'bottom 2))))

  ;; See `split-window-sensibly' and `window-splittable-p'
  (split-height-threshold 20 "10 lines minimal")
  (split-width-threshold 160 "80 columns minimal")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (help-window-select t "Make it easier to switch between focused window")
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t "Right and bottom")
  (window-divider-mode nil "Disabled by default")

;;;; Scrolling
  (scroll-conservatively 101)
  (scroll-step 1)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (hscroll-margin 3 "Avoid ending character overlapping in terminal mode")
  (hscroll-step 1)
  (auto-hscroll-mode t "set to 'current-line to scroll the current line only")
  (auto-window-vscroll t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(2 ((shift) . 0.5) ((control) . text-scale)))
  (redisplay-skip-fontification-on-input t)
  ;; Don't skip any characters
  (line-move-ignore-invisible nil)
  (line-move-visual nil)

;;;; Modeline
  (column-number-indicator-zero-based nil "Required by `doom-modeline'")
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (mode-line-compact 'long)
  (mode-line-position-column-format '(" C%C") "One-based column number")
  (mode-line-position-line-format '(" L%l"))
  (mode-line-position-column-line-format '(" %l:%C"))
  (mode-line-percent-position '(-3 "%o"))
  (mode-line-defining-kbd-macro '(:propertize " Macro" face mode-line-emphasis))
  ;; Simplify mode display
  (mode-line-modes (mapcar (lambda (x)
                             (cond
                              ;; Minimize minor mode indicator
                              ((and (listp x)
                                    (listp (cadr x))
                                    (eq 'minor-mode-alist (cadr (cadr x))))
                               '((defining-kbd-macro mode-line-defining-kbd-macro)))
                              ;; Remove parens around mode indicator
                              ((and (stringp x)
                                    (string-match-p "^[()]$" x))
                               nil)
                              (t x)))
                           mode-line-modes))

;;;; Line numbers
  (display-line-numbers-type 'relative)
  (global-hl-line-mode t)

;;;; Minibuffers
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

;;;; Tabbar
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice nil "Duplicate current tab")
  (tab-bar-position t)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-function #'tab-bar-tab-name-truncated)
  (tab-bar-show 1)
  (tab-bar-mode nil "Setting to t would cause display issue in terminal mode")

;;;;; Cursor
  (cursor-type 'box)
  (blink-cursor-mode nil)
  (mouse-yank-at-point t)
  (delete-selection-mode t)

;;;; Tabs
  (indent-tabs-mode nil)
  (tab-width 4)
  (tab-always-indent t "Hybrid indentation and completion with `complete'")
  (indent-line-function #'indent-relative) ;; indent-relative or insert-tab
  (backward-delete-char-untabify-method nil "Delete only one character at once")

;;;; Completion
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (bookmark-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycle-threshold nil "Always expand list")
  (completion-styles '(basic partial-completion))
  (completion-category-overrides '((file (styles basic partial-completion))))

;;;; Whitespaces
  ;; Leaving '(face ...) would cause confusion with `show-trailing-whitespace'
  (whitespace-style '(face trailing space-before-tab missing-newline-at-eof tab-mark))
  (show-trailing-whitespace t) ;; Independent to `whitespace-mode'
  (require-final-newline t) ;; Add missing newline on save

;;;; Line fold
  ;; No wrapping
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  ;; Fill columns
  (fill-column 80)
  (adaptive-fill-mode nil)
  (display-fill-column-indicator-column t) ;; Use `fill-column' variable

;;;; Encoding and locale
  (coding-system-for-write 'utf-8-unix)
  (buffer-file-coding-system 'utf-8-unix)
  (current-language-environment "UTF-8")
  (default-input-method "chinese-py")
  (inhibit-eol-conversion t)
  (display-raw-bytes-as-hex t)

;;;; Misc
  (use-short-answers t)
  (xterm-mouse-mode t)
  (context-menu-mode t)
  (shell-command-prompt-show-cwd t)
  (what-cursor-show-names t)
  (ring-bell-function 'ignore)
  (delete-by-moving-to-trash nil)
  ;; Log warnings in the background instead of poping the window up
  (native-comp-async-report-warnings-errors 'silent)
  ;; Pinentry -- Let Emacs use minibuffer to prompt passphrase
  (epg-pinentry-mode 'loopback)
  (compilation-scroll-output 'first-error)

;;;; File save
  (auto-save-default nil)
  (create-lockfiles nil)
  (make-backup-files nil)

;;;; Buffer auto refresh
  (global-auto-revert-mode t)

;;;; Clipboard
  (select-enable-clipboard t)
  (select-enable-primary t)

;;;; Pairs
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  (show-paren-mode t)

;;;; History persistence
  ;; Recentf mode
  (recentf-max-saved-items 250)
  (recentf-auto-cleanup 'never)
  (recentf-mode t)
  ;; Save history mode
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode t)
  ;; Save place mode
  (save-place-mode t)

;;;; Repeat mode
  (repeat-exit-key (kbd "C-g"))
  (repeat-exit-timeout 2)
  (repeat-mode t)

;;;; Dired
  (dired-listing-switches "-lahD --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer nil "Cannot open multiple dired windows if on")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)

;;;; Eshell
  (eshell-banner-message "")
  (eshell-history-size 1000)
  (eshell-save-history-on-exit t)
  (eshell-hist-ignoredups nil)
  (eshell-last-dir-ring-size 64)

;;;; ibuffer
  (ibuffer-movement-cycle nil)
  ;; Check `ibuffer-filtering-alist' for quilifiers.
  (ibuffer-saved-filter-groups `(("PEW"
                                  ("Doc" (or (mode . org-mode)
                                             (mode . markdown-mode)))
                                  ("Dired" (mode . dired-mode))
                                  ("Shell" (or (mode . shell-mode)
                                               (mode . eshell-mode)
                                               (mode . term-mode)
                                               (mode . vterm-mode)))
                                  ("Git" (name . ,(pewlib::workspace::buffer-regex :magit)))
                                  ("VC" (name . ,(pewlib::workspace::buffer-regex :vc)))
                                  ("Ediff" (name . ,(pewlib::workspace::buffer-regex :ediff)))
                                  ;; Putting to last to avoid buffers being wrongly categorized as "special"
                                  ("Special" (starred-name)))))

;;;; isearch and search
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (search-highlight t)
  (search-highlight-submatches t)
  (query-replace-highlight t)
  (query-replace-lazy-highlight t)
  (query-replace-highlight-submatches t)
  (query-replace-show-replacement t)

;;;; ispell
  (ispell-dictionary "en_US")

;;;; ediff
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)

;;;; electric
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-indent-mode nil)
  (electric-pair-mode nil)
  (electric-quote-mode nil)
  (electric-layout-mode nil)

;;;; TRAMP
  (tramp-default-method "scp")
  (tramp-remote-path '(tramp-own-remote-path tramp-default-remote-path))

;;;; Winner mode
  (winner-mode t)
  (winner-dont-bind-my-keys t)

;;; Switch commands
  :switch
  (indent-tabs-mode)
  (show-trailing-whitespace)
  (line-move-visual)
  (debug-on-error)
  (help-window-select)
  (display-line-numbers . (nil absolute relative visual))

;;; Transient keybindings
  :transient
  (pewkey
;;;; Take prefix
   ("C-u" . universal-argument)

;;;; Repeat
   ("C-r" . pewkey-repeat)

;;;; Windows
   ("q" . pewlib::workspace::close-window)
   ("1" . delete-other-windows)
   ("2" . pewlib::workspace::split-window-below)
   ("3" . pewlib::workspace::split-window-right)
   ("9" . window-toggle-side-windows)
   ("0" . pewlib::workspace::close-window)
   ("o" . pewlib::workspace::next-window)
   ("O" . pewlib::workspace::prev-window)
   ("h" . windmove-left)
   ("j" . windmove-down)
   ("k" . windmove-up)
   ("l" . windmove-right)
   ("C-<left>"  . shrink-window-horizontally)
   ("C-<down>"  . shrink-window)
   ("C-<up>"    . enlarge-window)
   ("C-<right>" . enlarge-window-horizontally)

;;;; Layout
   ("y" . winner-undo)
   ("Y" . winner-redo)

;;;; Other window
   ("C-v" . pewlib::workspace::scroll-other-window-page-down)
   ("M-v" . pewlib::workspace::scroll-other-window-page-up)
   ("C-n" . pewlib::workspace::scroll-other-window-line-down)
   ("C-p" . pewlib::workspace::scroll-other-window-line-up)
   ("C-l" . pewlib::workspace::recenter-other-window)

;;;; Tabbar
   ("Q"   . tab-bar-close-tab)
   ("R"   . tab-bar-rename-tab)
   ("f"   . tab-bar-switch-to-next-tab)
   ("F"   . tab-bar-select-tab-by-name)
   ("b"   . tab-bar-switch-to-prev-tab)
   ("t"   . tab-bar-new-tab)
   ("T"   . pewlib::workspace::pop-window-in-new-tab)
   ("C-t" . pewlib::workspace::pop-window-in-new-tab-persist)
   ("m"   . pewlib::workspace::move-tab-next)
   ("M"   . pewlib::workspace::move-tab-prev)

;;;; Buffers
   ("r" . rename-buffer)
   ("w" . save-buffer)
   ("n" . pewlib::workspace::next-editing-buffer)
   ("p" . pewlib::workspace::previous-editing-buffer)
   ("i" . pewlib::debug::display-buffer-file-name)
   ("B" . display-buffer)
   ("g" . revert-buffer-quick)

;;;; Jump
   ("C-o"   . pop-global-mark)
   ("M-."   . xref-find-definitions)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-go-back)
   ("C-M-," . xref-go-forward)
   ("M-?"   . xref-find-references)
   ("C-x"   . exchange-point-and-mark)
   ("SPC"   . set-mark-command)

;;;; Dabbrev completion
   ("M-/"   . dabbrev-expand)
   ("C-M-/" . dabbrev-completion)

;;;; Edit
   ("u" . undo)
   ("U" . undo-redo)
   (";" . comment-line)
   ("/" . isearch-forward-regexp)
   ("." . isearch-query-replace-regexp)
   ("=" . what-cursor-position)
   ("-" . ispell-word)

;;;; Zoom (zooming in/out depends on the last key.  see `text-scale-adjust')
   ("C-=" . text-scale-adjust)
   ("C--" . text-scale-adjust)
   ("C-0" . text-scale-adjust)

;;;; Frame Transparency
   ("M-=" . pewlib::workspace::increase-frame-opacity)
   ("M--" . pewlib::workspace::decrease-frame-opacity)
   ("A"   . pewlib::workspace::pop-window-in-new-frame)
   ("C-a" . pewlib::workspace::pop-window-in-new-frame-persist)
   ("a"   . other-frame)

;;;; Rebind word manipulations
   ("M-t" . transpose-words)
   ("M-c" . capitalize-word)
   ("M-u" . upcase-word)
   ("M-l" . downcase-word)
   ("M-z" . zap-to-char)
   ("M-q" . fill-paragraph)
   ("M-h" . mark-paragraph)

;;;; Editing
   ("DEL" . cycle-spacing))

  :map
  (pew::M-o-map)
  (pew::M-t-map)
  (pew::M-c-map)
  (pew::M-u-map
   ("a"   . org-capture)
   ("M-a" . org-agenda)
   ("d"   . flymake-show-buffer-diagnostics)
   ("D"   . flymake-show-project-diagnostics)
   ("M-d" . flymake-mode))
  (pew::M-l-map)
  (pew::M-z-map)
  (pew::M-q-map)
  (pew::M-h-map)

;;; Mode keybindings
  :bind
;;;; Global
  (global-map
   ;; Remap for better experience
   ([remap next-buffer] . pewlib::workspace::next-editing-buffer)
   ([remap previous-buffer] . pewlib::workspace::previous-editing-buffer)
   ([remap list-buffers] . ibuffer)
   ([remap isearch-delete-char] . isearch-del-char)

   ;; Tweak default window split logic
   ("C-x 2" . pewlib::workspace::split-window-below)
   ("C-x 3" . pewlib::workspace::split-window-right)

   ;; Pewkey
   ("C-z" . pewkey-map)

   ;; Less frequently used prefix that can be overriden
   ;; Reserved for the future
   ("M-o" . pew::M-o-map) ;; taken by minibuffer
   ("M-t" . pew::M-t-map)
   ("M-c" . pew::M-c-map) ;; taken by completion
   ("M-u" . pew::M-u-map) ;; taken by utilities
   ("M-l" . pew::M-l-map)
   ("M-z" . pew::M-z-map)
   ("M-q" . pew::M-q-map)
   ("M-h" . pew::M-h-map))

;;;; Dired
  (dired-mode-map
   ("RET"     . pewlib::extra::dired-go-to)
   ("DEL"     . pewlib::extra::dired-go-up)
   ("f"       . dired-find-file)
   ("b"       . dired-up-directory)
   ("<left>"  . dired-up-directory)
   ("<right>" . dired-find-file))

;;; Mode hooks
  :hook
  ;; Basic modes
  (prog-mode-hook . pewlib::editor::as-prog-mode)
  (text-mode-hook . pewlib::editor::as-text-mode)

  ;; Make shell clean
  (eshell-mode-hook . pewlib::editor::as-terminal-mode)
  (shell-mode-hook . pewlib::editor::as-terminal-mode)

  ;; Don't save trailing spaces
  (after-save-hook . pewlib::editor::delete-trailing-whitespaces)

  ;; Don't move cursor to the minibuffer prompt
  (minibuffer-setup-hook . cursor-intangible-mode)

  ;; Don't spawn new windows
  (grep-mode-hook . pewlib::workspace::reuse-window-in-buffer)

  ;; Show less in Dired
  (dired-mode-hook . dired-hide-details-mode)

;;; Symbol properties
  :property
  ;; Enable commands that are disabled by default
  (scroll-left
   (disabled . nil))

  (list-threads
   (disabled . nil))

  (list-timers
   (disabled . nil))

  (dired-find-alternate-file
   (disabled . nil))

  (upcase-region
   (disabled . nil))

  (downcase-region
   (disabled . nil))

  (narrow-to-region
   (disabled . nil))

;;; Face settings
  :face
  (default :family "Iosevka" :foundry "UKWN" :slant normal :weight normal :height 120 :width normal)
  (tab-bar :inherit default))

(provide 'init-config)
;;; init-config.el ends here
