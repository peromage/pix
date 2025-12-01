;;; pewlib-workspace.el --- Editor manipulations -*- lexical-binding: t; -*-

;; Author: Fang Deng <fang@elfang.com>

;;; Commentary:
;;; Code:

;;; Buffers
(defvar /p/buffer-regex-plist
  (let ((star "^ *\\*%s\\*.*$"))
    ;; Align regex  :[a-z-]+\(\s-*\)\((\|"\)
    (list
     ;; Generic
     :starred              (format star ".+")
     :non-starred          "^ *[^* ]"
     :with-leading-star    "^ *\\*"
     :with-leading-space   "^ +"
     ;; Extension buffers
     :magit                "^ *[Mm]agit"
     :vc                   (format star "[Vv][Cc][-_ ].+")
     :ediff                (format star "[Ee]diff[-_ ].+")
     :shell                (format star ".*[Ss]hell")
     :terminal             (format star ".*[Tt]erm\\(inal\\)?")
     :org-babel            (format star "[Oo]rg[-_ ][Bb]abel .+")
     :org-src              (format star "\\([Oo]rg[-_ ][Ss]rc .+\\|[Oo]rg .+ [Ss]rc\\)")
     :org-export           (format star "\\([Oo]rg[-_ ][Ee]xport .+\\|[Oo]rg .+ [Ee]xport\\)")
     :org-starred          (format star "\\([Oo]rg[-_ ].+\\|[Oo]rg\\)") ;; Generic
     :edit-indirect        (format star "edit-indirect[-_ ].+")
     ;; Common buffers
     :scratch              (format star "[Ss]cratch")
     :help                 (format star "[Hh]elp")
     :man                  (format star "[Mm]an .+")
     :woman                (format star "[Ww]o[Mm]an .+")
     :eldoc                (format star "[Ee]ldoc")
     :message              (format star "[Mm]essages?")
     :backtrace            (format star "[Bb]acktraces?")
     :warning              (format star "[Ww]arnings?")
     :log                  (format star "[Ll]ogs?")
     :compilation          (format star "[Cc]ompilations?")
     :output               (format star "[Oo]utputs?")
     :command              (format star "[Cc]ommands?")
     :tree-sitter-explorer (format star "[Tt]ree-sitter [Ee]xplorer ?.*")
     :flymake-diagnostics  (format star "[Ff]lymake [Dd]iagnostics ?.*")))
  "Buffer name patterns.")

(defvar /p/hidden-buffer-keywords
  '(:starred
    :with-leading-star
    :with-leading-space
    :magit)
  "Buffers that are treated as hidden.")

(defun /p/buffer-regex (keyword)
  "Get KEYWORD corresponded buffer regex from `pewlib-buffer-regex-plist'."
  (or (plist-get /p/buffer-regex-plist keyword)
      (error "Invalid keyword: %S" keyword)))

(defun /p/map-buffer-regex (keywords &optional concat)
  "Return a list of buffer regex from `pewlib-buffer-regex-plist' given KEYWORDS.
If CONCAT is non-nil the return value is a single regex string."
  (if concat
      (mapconcat #'/p/buffer-regex keywords "\\|")
    (mapcar #'/p/buffer-regex keywords)))

(defun /p/next-editing-buffer (&optional backwards)
  "Switch to the next editing buffer.
If BACKWARDS is non-nil switch to the previous one."
  (interactive "P")
  (let ((current-buffer (current-buffer))
        (switch-func (if backwards #'previous-buffer #'next-buffer)))
    (funcall switch-func)
    (while (and (not (eq current-buffer (current-buffer)))
                (or (string-match-p (/p/map-buffer-regex /p/hidden-buffer-keywords t) (buffer-name))
                    (/p/dired-buffer-p (buffer-name))))
      (funcall switch-func))))

(defun /p/previous-editing-buffer ()
  "Switch editing buffer backwards."
  (interactive)
  (/p/next-editing-buffer :previous))

(defun /p/close-other-buffers-in-major-mode (mode)
  "Close all other buffers in major MODE but this one."
  (interactive "SMajor mode: ")
  (let ((this-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (if (and (eq mode (buffer-local-value 'major-mode buffer))
               (not (eq this-buffer buffer)))
          (kill-buffer buffer)))))

;;; Windows
(defun /p/reuse-window-in-buffer ()
  "Make new spawned windows atttempt to reuse current ones.
This is usually useful in some major modes like `grep-mode'."
  (setq-local display-buffer-base-action '((display-buffer-reuse-window
                                            display-buffer-use-some-window))
              display-buffer-alist nil))

(defun /p/side-window-actions (side slot)
  "Return a list of pre-configured side window actions.
See `display-buffer' for property SIDE, SLOT."
  `((display-buffer-reuse-window display-buffer-in-side-window)
    (reusable-frames . t)
    (inhibit-switch-frame . t)
    (window-height . 0.25)
    (side . ,side)
    (slot . ,slot)))

(defun /p/side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (window-parameter window 'window-side))

(defun /p/side-window-exists-p (&optional side)
  "Return the first side window if there is any, otherwise nil.
If SIDE is given and is one of 'top' 'bottom' 'left' and 'right', check for that
specified side.  If SIDE is nil it means check all sides."
  (window-with-parameter 'window-side side))

(defun /p/normal-window-p (window)
  "Return t if WINDOW is a normal window."
  (not (/p/side-window-p window)))

(defun /p/last-normal-window-p (window)
  "Return t if WINDOW is the last normal window."
  (and (/p/normal-window-p window)
       (= 1 (length (/p/list-normal-windows)))))

(defun /p/list-side-windows ()
  "Return a list of side windows."
  (seq-filter
   (lambda (x) (/p/side-window-p x))
   (window-list)))

(defun /p/list-normal-windows ()
  "Return a list of normal (non-side) windows."
  (seq-filter
   (lambda (x) (/p/normal-window-p x))
   (window-list)))

(defun /p/pop-window-in-new-tab (arg)
  "Pop the current window into a new tab.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (/p/last-normal-window-p (selected-window))))
        (delete-window))
    (tab-bar-new-tab) ;; Duplicate current layout
    (select-window (car (/p/list-normal-windows)))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun /p/pop-window-in-new-tab-persist ()
  "Pop the current window and keep it in the original tab."
  (interactive)
  (/p/pop-window-in-new-tab :persist))

(defun /p/next-window ()
  "Switch to the next window."
  (interactive)
  (other-window 1))

(defun /p/prev-window ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun /p/close-window ()
  "Close the current window, or the tab if it is the last normal window."
  (interactive)
  (if (/p/last-normal-window-p (selected-window))
      ;; If there is only one normal window left, close the tab, regardless even
      ;; side windows exist
      (tab-bar-close-tab)
    (delete-window)))

(defun /p/scroll-other-window-page-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window))

(defun /p/scroll-other-window-page-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window-down))

(defun /p/scroll-other-window-line-down ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window 1))

(defun /p/scroll-other-window-line-up ()
  "Scroll other window one page down."
  (interactive)
  (scroll-other-window -1))

(defun /p/recenter-other-window ()
  "Scroll other window one page down."
  (interactive)
  (recenter-other-window))

(defun /p/split-window-below ()
  "Split windows and switch focus to the new one."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun /p/split-window-right ()
  "Split windows and switch focus to the new one."
  (interactive)
  (split-window-right)
  (other-window 1))

;;; Tabs
(defun /p/move-tab-next ()
  "Move current tab to the next."
  (interactive)
  (tab-bar-move-tab 1))

(defun /p/move-tab-prev ()
  "Move current tab to the previous."
  (interactive)
  (tab-bar-move-tab -1))

;;; Frames
(defvar /p/frame-opacity-adjust-step 5
  "The amount of opacity changed each time.")

(defun /p/set-frame-opacity (val)
  "Set the opacity of the current frame.
VAL is a number between 0 and 100.  0=transparent/100=opaque"
  (interactive "nFrame Opacity [transparent(0) - opaque(100)]: ")
  (let ((value (cond ((> val 100) 100)
                     ((< val 0) 0)
                     (t val))))
    (message "Set Frame opacity: %d%%" value)
    (set-frame-parameter (selected-frame) 'alpha (cons value value))))

(defun /p/increase-frame-opacity ()
  "Increase frame opacity."
  (interactive)
  (/p/set-frame-opacity (+ (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                /p/frame-opacity-adjust-step)))

(defun /p/decrease-frame-opacity ()
  "Decrease frame opacity."
  (interactive)
  (/p/set-frame-opacity (- (car (or (frame-parameter (selected-frame) 'alpha) '(100 . nil)))
                                /p/frame-opacity-adjust-step)))

(defun /p/pop-window-in-new-frame (arg)
  "Pop the current window into a new frame.
If prefix ARG is presented, pop the window without deleting it from the original
place."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (and (null arg) (not (/p/last-normal-window-p (selected-window))))
        (delete-window))
    (select-frame (make-frame-command))
    (switch-to-buffer current-buffer)
    (delete-other-windows)))

(defun /p/pop-window-in-new-frame-persist ()
  "Pop the current window and keep it in the original frame."
  (interactive)
  (/p/pop-window-in-new-frame :persist))

(provide 'pewlib-workspace)
;;; pewlib-workspace.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("/p/" . "pewlib-"))
;; End:
