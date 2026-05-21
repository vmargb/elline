;;; elline.el --- Beautiful, theme-adaptive, modular modeline -*- lexical-binding: t -*-

;;; Commentary:
;; A small and lightweight mode-line with the same style as
;; doom-modeline, but without the extra fluff.

;; Features:
;; - Flymake/Flycheck diagnostics with all severity levels (dimmed if 0)
;; - Static height configuration via `elline-height`
;; - Lualine-style separators (none, arrow, curve, slant) with dynamic transitions
;; - Style toggle (C-c m b) to switch between 'flat and 'blocks
;; - Evil mode state indicator with Nerd Icons
;; - Inactive windows are visibly dimmed
;; - Modular left, center, and right zone joiners for seamless segments
;; - Idle-cache rendering: no redraws during scrolling/commands, only on idle

;;; Code:

(require 'subr-x)
(require 'color)
(require 'cl-lib)

;; Customization
;; -------------------------------------------------------------------------

(defgroup elline nil
  "Beautiful, adaptive mode-line."
  :group 'elline)

(defcustom elline-icon-provider 'labels
  "Icon backend: `nerd-icons', `all-the-icons', `labels', or `none'."
  :type '(choice (const nerd-icons)
                 (const all-the-icons)
                 (const labels)
                 (const none))
  :group 'elline)

(defcustom elline-responsive t
  "If non-nil, hide 'non-essential' segments in narrow windows."
  :type 'boolean
  :group 'elline)

(defcustom elline-show-time nil
  "If non-nil, show HH:MM at the far right."
  :type 'boolean
  :group 'elline)

(defcustom elline-height 160
  "Height of the modeline (120 is default font size)."
  :type 'integer
  :group 'elline)

(defcustom elline-theme-style 'flat
  "Theme style is either `flat' or `blocks'."
  :type '(choice (const flat)
                 (const blocks))
  :group 'elline)

(defcustom elline-separator-height 0.95
  "Relative height for separators.
Also acts as the pivot point for vertical alignment."
  :type 'float
  :group 'elline)

(defcustom elline-separator-style 'none
  "Style of zone separators.  Only visible if `elline-theme-style' is `blocks'."
  :type '(choice (const none)
                 (const arrow)
                 (const curve)
                 (const slant)
                 (const slant-forward))
  :group 'elline)

(defvar elline-separators
  '((arrow . ("" . ""))
    (curve . ("" . ""))
    (slant . ("" . ""))
    (slant-forward . ("" . "")))
  "Alist mapping separator styles to their (left . right) glyphs.")

(defcustom elline-separator-raise 0
  "Vertical adjustment for separator glyphs.
Negative values lower the glyph, positive values raise it."
  :type 'float
  :group 'elline)

(defcustom elline-show-project t
  "If non-nil, show the current project or tab name in the mode-line."
  :type 'boolean
  :group 'elline)

(defcustom elline-idle-delay 0.1
  "Seconds of idle delay before the mode-line is allowed to redraw.
During commands and scrolling the mode-line is frozen at its last
cached value until the user pauses for this long the cache is
invalidated and a single redraw is triggered."
  :type 'number
  :group 'elline)

;; height cache
(defvar elline--cached-abs-height nil
  "Cached absolute height to avoid repeated floor calculations.")

(defun elline--update-abs-height ()
  "Recalculate and cache the absolute segment height."
  (setq elline--cached-abs-height (floor (* elline-height elline-separator-height))))

;; Idle-render cache
;; -------------------------------------------------------------------------
;; Three buffer-local variables are used in the debounce strategy:
;;   elline--cached-line    the last fully-built mode-line value
;;   elline--cache-key      the state snapshot that produced it
;;   elline--refresh-timer  pending idle timer (one per buffer)
;;
;; elline--render is called by (:eval …) on every display pass.  It
;; returns the cached value immediately unless the key has changed.
;;
;; After every command / scroll event, elline--schedule-refresh cancels
;; any ongoing timer and arms a new one(debouncing).
;; Only when the user actually stops does the timer fire. Then
;; force-mode-line-update does exactly one redraw.
;;
;; A separate 60-second repeating timer keeps the clock accurate even
;; when the user is completely idle.

(defvar-local elline--cached-line nil
  "Last built line value for this buffer.")

(defvar-local elline--cache-key nil
  "State snapshot that produced `elline--cached-line'.")

(defvar-local elline--refresh-timer nil
  "Debounce timer for this buffer.")

(defvar elline--clock-timer nil
  "Repeating 60-second timer that keeps the HH:MM display accurate.")

(defun elline--compute-cache-key ()
  "Return a snapshot of all state that affects the mode-line."
  (list (window-width)
        (elline--active-p)
        elline-theme-style
        elline-separator-style
        elline-show-time
        ;; only include the current minute when the clock segment is on,
        ;; so the display advances without needing a cursor movement.
        (when elline-show-time (format-time-string "%H:%M"))
        buffer-read-only
        (buffer-modified-p)
        vc-mode
        major-mode
        elline-icon-provider
        elline-show-project
        (elline--current-tab-name)
        (ignore-errors
          (let ((proj (project-current nil)))
            (when proj (project-root proj))))))

(defun elline--render ()
  "Return the cached mode-line, rebuilding only when state has changed."
  (let ((key (elline--compute-cache-key)))
    (unless (equal key elline--cache-key)
      (setq elline--cache-key  key
            elline--cached-line (elline--build)))
    elline--cached-line))

(defun elline--schedule-refresh (&rest _)
  "Debounce line redraws, cancel any pending timer and create a new one.
The line will not be redrawn until the idle for `elline-idle-delay' seconds"
  (when elline--refresh-timer
    (cancel-timer elline--refresh-timer))
  (setq elline--refresh-timer
        (run-with-idle-timer
         elline-idle-delay nil
         ;; Capture the buffer so the cache-key is nilled in the right place
         ;; even if focus has moved by the time the timer fires.
         (let ((buf (current-buffer)))
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq elline--refresh-timer nil
                       elline--cache-key     nil)))
             (force-mode-line-update t))))))

(defun elline--start-clock-timer ()
  "Start or restart the 60-second repeating timer for the clock segment."
  (elline--stop-clock-timer)
  (setq elline--clock-timer
        (run-with-timer 60 60
                        (lambda ()
                          ;; invalidate every live buffer so the minute
                          ;; ticks over wherever it is displayed.
                          (dolist (buf (buffer-list))
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (setq elline--cache-key nil))))
                          (force-mode-line-update t)))))

(defun elline--stop-clock-timer ()
  "Cancel the clock refresh timer if it is running."
  (when elline--clock-timer
    (cancel-timer elline--clock-timer)
    (setq elline--clock-timer nil)))


;; Theme-adaptive colour handling
;; -------------------------------------------------------------------------

(defun elline--active-p ()
  "Return t if rendering the selected window's mode-line."
  (eq (window-buffer (selected-window)) (current-buffer)))

(defun elline--blend (c1 c2 alpha)
  "Blend hex colour C1 toward C2 by ALPHA (0.0–1.0)."
  (condition-case nil
      (apply #'color-rgb-to-hex
             (cl-mapcar (lambda (a b) (+ (* (- 1 alpha) a) (* alpha b)))
                        (color-name-to-rgb c1)
                        (color-name-to-rgb c2)))
    (error c1)))

(defvar elline--build-colors nil
  "Plist of pre-computed colors, bound dynamically during `elline--build'.")

(defun elline--snapshot-colors ()
  "Compute all theme colors in one pass.  Call once per build."
  (let* ((active  (elline--active-p))
         (blocks-p (eq elline-theme-style 'blocks))
         (ml-face  (if active 'mode-line 'mode-line-inactive))
         (bg       (or (face-attribute ml-face :background nil 'default) "#1e1e2e"))
         (fg       (or (face-attribute ml-face :foreground nil 'default) "#cdd6f4"))
         (accent   (or (face-attribute 'font-lock-keyword-face :foreground nil 'default) "#89b4fa"))
         (warning  (or (face-attribute 'warning :foreground nil 'default) "#f9e2af"))
         (error-c  (or (face-attribute 'error   :foreground nil 'default) "#f38ba8"))
         (success  (or (face-attribute 'success :foreground nil 'default) "#a6e3a1"))
         (rgb      (color-name-to-rgb bg))
         (light    (and rgb (> (apply #'+ rgb) 1.5))))
    (list 'bg-main  bg
          'fg       fg
          'accent   accent
          'warning  warning
          'error    error-c
          'success  success
          'bg-alt   (if blocks-p (elline--blend bg accent (if active (if light 0.22 0.12) (if light 0.12 0.06))) bg)
          'bg-alt2  (if blocks-p (elline--blend bg accent (if active (if light 0.40 0.25) (if light 0.22 0.12))) bg)
          'bg-neutral (if blocks-p (elline--blend bg fg (if active (if light 0.18 0.10) (if light 0.10 0.05))) bg)
          'fg-dim   (elline--blend fg bg (if active (if light 0.35 0.55) (if light 0.45 0.65)))
          'fg-inactive (elline--blend fg bg (if light 0.40 0.60)))))

(defun elline--color (key)
  "Return theme color for KEY.  Use per-build snapshot when available."
  (or (plist-get elline--build-colors key)
      ;; Fallback for calls outside a build (e.g. active-bar propertize)
      (progn (unless elline--build-colors
               (setq elline--build-colors (elline--snapshot-colors)))
             (plist-get elline--build-colors key))))

;; Icon selection
;; -------------------------------------------------------------------------

(defun elline--icon (family name &optional fallback)
  "For each icon-provider, return an icon glyph for FAMILY with NAME."
  (let ((fallback (or fallback "")))
    (condition-case nil
        (let ((icon (pcase elline-icon-provider
                      ('nerd-icons
                       (pcase family
                         ('oct     (nerd-icons-octicon  name))
                         ('fa      (nerd-icons-faicon   name))
                         ;; auto-prefix "md_" for nerd-icons material icons
                         ('md      (nerd-icons-mdicon   (if (string-prefix-p "md_" name) name (concat "md_" name))))
                         ('codicon (nerd-icons-codicon  name))
                         ('file    (or (nerd-icons-icon-for-file (or buffer-file-name (buffer-name)))
                                       (nerd-icons-faicon "file")))
                         ('mode    (or (nerd-icons-icon-for-mode major-mode)
                                       (nerd-icons-codicon "code")))))
                      ('all-the-icons
                       (pcase family
                         ('oct     (all-the-icons-octicon name))
                         ('fa      (all-the-icons-faicon name))
                         ;; auto-strip "md_" for all-the-icons material icons
                         ('md      (all-the-icons-material (replace-regexp-in-string "^md_" "" name)))
                         ('file    (all-the-icons-icon-for-file (or buffer-file-name (buffer-name))))
                         ('mode    (all-the-icons-icon-for-mode major-mode))
                         (_        (all-the-icons-faicon (or name "cog")))))
                      (_ fallback))))
          ;; handle cases where the icon library returns nil or ""
          (or (and (stringp icon) (not (string-empty-p icon)) icon) fallback))
      (error fallback))))

(defun elline--segment-marker (label family name &optional fallback)
  "Return LABEL in `labels' mode, otherwise an icon for FAMILY/NAME."
  (if (eq elline-icon-provider 'labels)
      label
    (elline--icon family name fallback)))

(defun elline--label-prefix (label text)
  "TEXT with LABEL in `labels' mode."
  (if (eq elline-icon-provider 'labels)
      (format "%s %s" label text)
    text))

(defun elline--current-tab-name ()
  "Return the current tab name, or nil when tabs are unavailable.
And suppress the tab when its default/unnamed."
  (let* ((tabs (or (and (fboundp 'tab-bar-tabs) (tab-bar-tabs))
                   (frame-parameter nil 'tabs)))
         (tab  (or (cl-find-if (lambda (tab) (eq (car tab) 'current-tab)) tabs)
                   (car tabs)))
         (name (and tab (alist-get 'name tab))))
    (when (and (stringp name)
               (not (string-empty-p name))
               (not (member (downcase name) '("default" "tab 1"))))
      name)))

(defun elline--tab ()
  "Current tab segment for the left side of the modeline.
Hidden for the default unnamed tab."
  (when (> (window-width) 30)
    (let ((name (elline--current-tab-name)))
      (when name
        (elline--seg name
                     (elline--color 'bg-alt)
                     (elline--color 'accent)
                     (elline--segment-marker "Tab:" 'oct "square" "▣")
                     t)))))

;; Segment & Separator logic
;; -------------------------------------------------------------------------

(defun elline--seg (text &optional bg fg icon bold)
  "Return a propertized mode-line segment with TEXT.
Uses an absolute integer height to prevent icon scaling from stretching the line."
  (let* ((bg (or bg (elline--color 'bg-main)))
         (fg (or fg (elline--color 'fg)))
         (abs-height (or elline--cached-abs-height ;; height cache
                         (floor (* elline-height elline-separator-height))))
         (base-face `(:background ,bg :foreground ,fg :height ,abs-height ,@(when bold '(:weight bold))))
         (res (concat " " (when icon (concat icon " ")) text " ")))
    (add-face-text-property 0 (length res) base-face nil res)
    res))

(defun elline--get-bg (str default-bg)
  "Extract the DEFAULT-BG colour from the first character of STR."
  (if (and str (> (length str) 0))
      (let ((face (get-text-property 0 'face str)))
        (cond
         ((listp face) (or (plist-get face :background) default-bg))
         ((facep face) (face-attribute face :background nil 'default))
         (t default-bg)))
    default-bg))

;; Note: join-left and join-right are updated to now use a
;; parts list which prevents the old O(n^2) string concatenation to O(n)
(defun elline--join-left (segs default-bg)
  (let* ((blocks-p  (eq elline-theme-style 'blocks))
         (sep-glyph (and blocks-p
                         (not (eq elline-separator-style 'none))
                         (car (alist-get elline-separator-style elline-separators))))
         (parts    '())
         (prev-bg   default-bg))
    (dolist (seg segs)
      (when (and seg (not (string-empty-p seg)))
        (let ((cur-bg (elline--get-bg seg default-bg)))
          (when (and sep-glyph (not (string= prev-bg cur-bg)) parts)
            (push (propertize sep-glyph
                              'face    `(:foreground ,prev-bg :background ,cur-bg :height ,elline-separator-height)
                              'display `((raise . ,elline-separator-raise)))
                  parts))
          (push seg parts)
          (setq prev-bg cur-bg))))
    (when (and sep-glyph (not (string= prev-bg default-bg)))
      (push (propertize sep-glyph
                        'face    `(:foreground ,prev-bg :background ,default-bg :height ,elline-separator-height)
                        'display `((raise . ,elline-separator-raise)))
            parts))
    (apply #'concat (nreverse parts))))

(defun elline--join-right (segs default-bg)
  "Join SEGS with right-pointing separators."
  (let* ((blocks-p (eq elline-theme-style 'blocks))
         (sep-glyph (and blocks-p
                         (not (eq elline-separator-style 'none))
                         (cdr (alist-get elline-separator-style elline-separators))))
         (parts '())
         (left-bg default-bg))
    (dolist (seg segs)
      (when (and seg (not (string-empty-p seg)))
        (let ((right-bg (elline--get-bg seg default-bg)))
          (when (and sep-glyph (not (string= left-bg right-bg)))
            (push (propertize sep-glyph
                              'face `(:foreground ,right-bg :background ,left-bg :height ,elline-separator-height)
                              'display `((raise . ,elline-separator-raise)))
                  parts))
          (push seg parts)
          (setq left-bg right-bg))))
    (apply #'concat (nreverse parts))))

;; Segments
;; -------------------------------------------------------------------------

(defun elline--evil ()
  "Evil mode state pill."
  (when (bound-and-true-p evil-mode)
    (let* ((active   (elline--active-p))
           (blocks-p (eq elline-theme-style 'blocks))
           (state    (or evil-state 'normal))
           (label    (pcase state
                       ('normal   "NOR")
                       ('insert   "INS")
                       ('visual   "VIS")
                       ('replace  "REP")
                       ('operator "OPR")
                       ('motion   "MOT")
                       ('emacs    "EMC")
                       (_         "???")))
           (state-c  (pcase state
                       ('normal   (elline--color 'accent))
                       ('insert   (elline--color 'success))
                       ('visual   (elline--color 'warning))
                       ('replace  (elline--color 'error))
                       ('operator (elline--color 'warning))
                       ('motion   (elline--color 'accent))
                       ('emacs    (elline--color 'fg-dim))
                       (_         (elline--color 'fg))))
           (bg       (if blocks-p (if active state-c (elline--color 'bg-alt2)) (elline--blend (elline--color 'bg-main) "#000000" 0.10)))
           (fg       (if blocks-p (if active (elline--color 'bg-main) (elline--color 'fg-dim)) state-c))
           (boxed    (concat "  " label "  ")))
      (propertize boxed 'face `(:background ,bg :foreground ,fg :weight bold)))))

(defun elline--winum ()
  (when (and (fboundp 'winum-get-number) (> (window-width) 20))
    (let ((n (winum-get-number)))
      (when n
        (elline--seg (elline--label-prefix "Win:" (number-to-string n))
                     (elline--color 'bg-alt)
                     (elline--color 'accent))))))

(defun elline--status ()
  (let* ((ro     buffer-read-only)
         (mod    (and (buffer-modified-p) (not ro)))
         (narrow (buffer-narrowed-p))
         (label  (cond (ro "Read-only")
                       (mod "Modified")
                       (narrow "Narrowed")
                       (t "Clean")))
         (icon   (cond (ro     (elline--segment-marker "State:" 'oct "lock" ""))
                       (mod    (elline--segment-marker "State:" 'oct "dot_fill" "●"))
                       (narrow (elline--segment-marker "State:" 'oct "screen_full" "▼"))
                       (t      (elline--segment-marker "State:" 'oct "dot" "○"))))
         (fg     (cond (ro     (elline--color 'error))
                       (mod    (elline--color 'warning))
                       (t      (elline--color 'fg)))))
    (elline--seg (if (eq elline-icon-provider 'labels) label "")
                 (elline--color 'bg-alt) fg icon)))

(defun elline--buffer-name ()
  (let* ((name   (buffer-name))
         (remote (file-remote-p default-directory))
         (display (if remote (concat "Remote " name) name)))
    ;; `nil` for the icon argument to prevent duplicate file icons
    (elline--seg display (elline--color 'bg-alt2) (elline--color 'fg) nil t)))

(defvar-local elline--cached-project-name nil)
(defvar-local elline--cached-project-root nil)

(defun elline--update-project-cache ()
  (let ((proj (ignore-errors (project-current))))
    (if proj
        (let ((root (project-root proj)))
          (unless (equal root elline--cached-project-root)
            (setq elline--cached-project-root root
                  elline--cached-project-name
                  (if (fboundp 'project-name) (project-name proj) (file-name-nondirectory (directory-file-name root))))))
      (setq elline--cached-project-root nil elline--cached-project-name nil))))

(add-hook 'find-file-hook #'elline--update-project-cache)
(add-hook 'dired-mode-hook #'elline--update-project-cache)
(add-hook 'after-change-major-mode-hook #'elline--update-project-cache)
;; (add-hook 'buffer-list-update-hook #'elline--update-project-cache)

(defun elline--project ()
  "Current project segment, shown independently from tabs."
  (when (and elline-show-project (> (window-width) 40))
    (unless elline--cached-project-root (elline--update-project-cache))
    (when elline--cached-project-name
      (elline--seg elline--cached-project-name
                   (elline--color 'bg-alt2)
                   (elline--color 'accent)
                   (elline--segment-marker "Proj:" 'oct "repo" "📁")
                   t))))

(defun elline--git ()
  (when (and vc-mode (stringp vc-mode) (string-match-p "^ Git" vc-mode))
    (let* ((branch (replace-regexp-in-string "^ Git[:-]?\\s-*" "" vc-mode))
           (dirty  (string-match-p "^ Git[:*]" vc-mode))
           (icon   (elline--segment-marker "Git:" 'oct "git_branch" "")))
      (elline--seg branch (elline--color 'bg-alt)
                   (if dirty (elline--color 'warning) (elline--color 'fg))
                   icon))))

(defun elline--macro ()
  (when defining-kbd-macro
    (elline--seg (if (eq elline-icon-provider 'labels) "Recording" "REC")
                 (elline--color 'error)
                 (elline--color 'bg-main)
                 (elline--segment-marker "Recording:" 'fa "circle" "●")
                 t)))

(defun elline--selection ()
  (when (and (use-region-p) (> (window-width) 80))
    (elline--seg (elline--label-prefix
                  "Selection:"
                  (format "%dL %dC"
                          (count-lines (region-beginning) (region-end))
                          (abs (- (region-end) (region-beginning)))))
                 (elline--color 'bg-alt)
                 (elline--color 'fg))))

(defun elline--major-mode ()
  (let* ((name (capitalize (string-trim (symbol-name major-mode) nil "-mode$")))
         (icon (elline--segment-marker "Type:" 'mode nil "")))
    (elline--seg name (elline--color 'bg-alt) (elline--color 'fg) icon)))

(defun elline--process ()
  (when mode-line-process
    (let ((str (format-mode-line mode-line-process)))
      (unless (string-blank-p str)
        (elline--seg (elline--label-prefix "Proc:" str)
                     (elline--color 'bg-alt2)
                     (elline--color 'accent))))))

(defun elline--lsp ()
  (when (> (window-width) 60)
    (when (or (bound-and-true-p lsp-mode) (bound-and-true-p eglot--managed-mode))
      (elline--seg (if (eq elline-icon-provider 'labels) "Active" "LSP")
                   (elline--color 'bg-alt)
                   (elline--color 'success)
                   (elline--segment-marker "LSP:" 'codicon "symbol_method" "λ")))))

(defun elline--flycheck ()
  (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-count-errors) (> (window-width) 50))
    (let* ((counts (when flycheck-current-errors (flycheck-count-errors flycheck-current-errors)))
           (e (or (alist-get 'error counts) 0))
           (w (or (alist-get 'warning counts) 0))
           (i (or (alist-get 'info counts) 0))
           (bg (elline--color 'bg-alt)))
      (concat
       (elline--seg (number-to-string e) bg (if (> e 0) (elline--color 'error) (elline--color 'fg-dim))
                    (elline--segment-marker "Err:" 'oct "x_circle" "✖") t)
       (elline--seg (number-to-string w) bg (if (> w 0) (elline--color 'warning) (elline--color 'fg-dim))
                    (elline--segment-marker "Warn:" 'oct "alert" "⚠") t)
       (elline--seg (number-to-string i) bg (if (> i 0) (elline--color 'success) (elline--color 'fg-dim))
                    (elline--segment-marker "Info:" 'oct "info" "ℹ") t)))))

(defun elline--flymake-counts ()
  "Return (errors warnings notes) in a single pass over diagnostics."
  (let ((e 0) (w 0) (n 0))
    (dolist (d (flymake-diagnostics))
      (let* ((dt  (flymake-diagnostic-type d))
             (cat (or (get dt 'flymake-category) dt)))
        (cond ((or (eq cat 'flymake-error)   (eq dt :error))   (cl-incf e))
              ((or (eq cat 'flymake-warning) (eq dt :warning)) (cl-incf w))
              ((or (eq cat 'flymake-note)    (eq dt :note))    (cl-incf n)))))
    (list e w n)))

(defun elline--flymake ()
  (when (and (bound-and-true-p flymake-mode) (> (window-width) 50))
    (pcase-let ((`(,e ,w ,i) (elline--flymake-counts))
                (bg (elline--color 'bg-alt)))
      (concat
       (elline--seg (number-to-string e) bg (if (> e 0) (elline--color 'error)   (elline--color 'fg-dim))
                    (elline--segment-marker "Err:" 'oct "x_circle" "✖") t)
       (elline--seg (number-to-string w) bg (if (> w 0) (elline--color 'warning) (elline--color 'fg-dim))
                    (elline--segment-marker "Warn:" 'oct "alert" "⚠") t)
       (elline--seg (number-to-string i) bg (if (> i 0) (elline--color 'success) (elline--color 'fg-dim))
                    (elline--segment-marker "Info:" 'oct "info" "ℹ") t)))))

(defun elline--encoding ()
  (when (and buffer-file-coding-system (> (window-width) 40))
    (let* ((sys      (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol      (pcase eol-type (0 "LF") (1 "CRLF") (2 "CR") (_ "??")))
           (coding   (cond ((string-match-p "utf-8" sys) "UTF-8")
                           ((string-match-p "latin-1" sys) "LATIN-1")
                           (t (upcase sys)))))
      (elline--seg (elline--label-prefix "Enc:" (format "%s %s" coding eol))
                   (elline--color 'bg-alt2)
                   (elline--color 'fg)))))

(defun elline--position ()
  (elline--seg (elline--label-prefix "Pos:" (format "%d:%d" (line-number-at-pos) (current-column)))
               (elline--color 'bg-alt) (elline--color 'fg)))

(defun elline--percentage ()
  (when (> (window-width) 50)
    (elline--seg (elline--label-prefix "Perc:" (format-mode-line "%p"))
                 (elline--color 'bg-alt2) (elline--color 'fg-dim))))

(defun elline--buffer-size ()
  (when (> (window-width) 60)
    (let ((size (buffer-size)))
      (elline--seg
       (elline--label-prefix
        "Size:"
        (cond ((> size 1048576) (format "%.1fM" (/ size 1048576.0)))
              ((> size 1024)    (format "%.1fk" (/ size 1024.0)))
              (t                (format "%dB" size))))
       (elline--color 'bg-alt)
       (elline--color 'fg-dim)))))

(defun elline--time ()
  (when (and elline-show-time (> (window-width) 70))
    (elline--seg (format-time-string "%H:%M")
                 (elline--color 'bg-alt2)
                 (elline--color 'accent)
                 (elline--segment-marker "Time:" 'oct "clock" "⏱"))))

;; Extensibility API
;; -------------------------------------------------------------------------

(defvar elline-left-segments
  '(elline--active-bar
    elline--evil
    elline--winum
    elline--status
    elline--tab
    elline--project
    elline--git
    elline--buffer-name
    elline--macro
    elline--selection
    elline--major-mode
    ;; elline--git
    elline--process))

(defvar elline-right-segments
  '(elline--lsp elline--flycheck
                elline--flymake
                elline--encoding
                elline--position
                elline--percentage
                elline--buffer-size
                elline--time))

(defmacro elline-def-segment (name bg fg &rest body)
  "Define a new modeline segment named NAME. Passes raw string to preserve icon fonts."
  `(defun ,name ()
     (let ((str (progn ,@body)))
       (when (and (stringp str) (not (string-blank-p str)))
         (elline--seg str (elline--color ,bg) (elline--color ,fg))))))

(defun elline-add-segment (section segment &optional after-segment)
  (let* ((list-var (if (eq section 'left) 'elline-left-segments 'elline-right-segments))
         (list-val (symbol-value list-var)))
    (if after-segment
        (let ((pos (cl-position after-segment list-val)))
          (if pos (set list-var (append (cl-subseq list-val 0 (1+ pos)) (list segment) (cl-subseq list-val (1+ pos)))) (add-to-list list-var segment t)))
      (add-to-list list-var segment t))))

;; Zones
;; -------------------------------------------------------------------------

(defun elline--active-bar ()
  (when (and (elline--active-p) (eq elline-theme-style 'flat))
    (propertize "▌" 'face `(:foreground ,(elline--color 'accent) :background ,(elline--color 'bg-main)))))

(defun elline--build-left (default-bg w)
  (elline--join-left (delq nil (mapcar (lambda (f) (when (fboundp f) (funcall f))) elline-left-segments)) default-bg))

(defun elline--build-right (default-bg w)
  (elline--join-right (delq nil (mapcar (lambda (f) (when (fboundp f) (funcall f))) elline-right-segments)) default-bg))

;; Main setup
;; -------------------------------------------------------------------------

(defun elline--right-align-width (str)
  "Calculate exact pixel width for robust right alignment."
  (if (fboundp 'string-pixel-width)
      (list (string-pixel-width str))
    (string-width str)))

(defun elline--build ()
  (let* ((elline--build-colors (elline--snapshot-colors))
         (w           (window-width))
         (default-bg  (elline--color 'bg-main))
         (left        (elline--build-left default-bg w))
         (right       (elline--build-right default-bg w))
         (align-width (if right (elline--right-align-width right) 0)))
    (list left
          `(:propertize " " display ((space :align-to (- (+ right right-fringe right-margin) ,align-width))))
          right)))

(defun elline--apply-height ()
  "Apply height or refresh cache whenever height is applied."
  (elline--update-abs-height)
  (set-face-attribute 'mode-line nil :height elline-height)
  (set-face-attribute 'mode-line-inactive nil :height elline-height)
  (force-mode-line-update t))

;; Interactive commands
;; -------------------------------------------------------------------------
;; each command nils the cache key so the next redisplay (triggered by
;; force-mode-line-update) picks up the change immediately, bypassing the
;; normal debounce path.

(defun elline--invalidate-cache-now ()
  "Nil the cache key in all live buffers and force an immediate redisplay.
Use this after interactive setting changes so the effect is instant."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq elline--cache-key nil))))
  (force-mode-line-update t))

(defun elline-cycle-icons ()
  (interactive)
  (setq elline-icon-provider
        (pcase elline-icon-provider
          ('nerd-icons (progn (message "Switching to all-the-icons") 'all-the-icons))
          ('all-the-icons (progn (message "Switching to labels") 'labels))
          ('labels (progn (message "Switching to none") 'none))
          ('none (progn (message "Switching to nerd-icons") 'nerd-icons))))
  (elline--invalidate-cache-now))

(defun elline-toggle-time ()
  (interactive)
  (setq elline-show-time (not elline-show-time))
  (elline--invalidate-cache-now))

(defun elline-toggle-style ()
  (interactive)
  (setq elline-theme-style (if (eq elline-theme-style 'flat) 'blocks 'flat))
  (elline--invalidate-cache-now))

(defun elline-cycle-separators ()
  (interactive)
  (setq elline-separator-style
        (pcase elline-separator-style
          ('none 'arrow) ('arrow 'curve) ('curve 'slant) ('slant 'slant-forward) ('slant-forward 'none) (_ 'none)))
  (elline--invalidate-cache-now))

(defun elline--refresh-all-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf (force-mode-line-update)))
  (redisplay t))

(defvar elline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m i") #'elline-cycle-icons)
    (define-key map (kbd "C-c m t") #'elline-toggle-time)
    (define-key map (kbd "C-c m s") #'elline-cycle-separators)
    (define-key map (kbd "C-c m b") #'elline-toggle-style)
    map))

;;;###autoload
(define-minor-mode elline-mode
  "Toggle elline globally."
  :global t
  :lighter nil
  :keymap elline-mode-map
  (if elline-mode
      (progn
        (setq-default mode-line-format '("%e" (:eval (elline--render))))
        (add-hook 'post-command-hook     #'elline--schedule-refresh)
        (add-hook 'window-scroll-functions #'elline--schedule-refresh)
        (when elline-show-time (elline--start-clock-timer))
        (elline--apply-height)
        (elline--refresh-all-buffers))
    ;; remove hooks, cancel all pending timers and restore format
    (remove-hook 'post-command-hook      #'elline--schedule-refresh)
    (remove-hook 'window-scroll-functions #'elline--schedule-refresh)
    (elline--stop-clock-timer)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when elline--refresh-timer
            (cancel-timer elline--refresh-timer)
            (setq elline--refresh-timer nil)))))
    (setq-default mode-line-format (default-value 'mode-line-format))
    (elline--refresh-all-buffers)))

(provide 'elline)
;;; elline.el ends here
