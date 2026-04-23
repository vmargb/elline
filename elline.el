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

;;; Code:

(require 'subr-x)
(require 'color)
(require 'cl-lib)

;; Customization
;; -------------------------------------------------------------------------

(defgroup elline nil
  "Beautiful, adaptive mode-line."
  :group 'elline)

(defcustom elline-icon-provider 'nerd-icons
  "Icon backend: `nerd-icons', `all-the-icons', or `none'."
  :type '(choice (const nerd-icons)
                 (const all-the-icons)
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

(defcustom elline-height 120
  "Height of the modeline (120 is default font size)."
  :type 'integer
  :group 'elline)

(defcustom elline-theme-style 'flat
  "Theme style is either `flat' or `blocks'."
  :type '(choice (const flat)
                 (const blocks))
  :group 'elline)

(defcustom elline-separator-style 'none
  "Style of zone separators.  Only visible if `elline-theme-style' is `blocks'."
  :type '(choice (const none)
                 (const arrow)
                 (const curve)
                 (const slant))
  :group 'elline)

(defvar elline-separators
  '((arrow . ("" . ""))
    (curve . ("" . ""))
    (slant . ("" . "")))
  "Alist mapping separator styles to their (left . right) glyphs.")

(defcustom elline-separator-raise -0.1
  "Vertical adjustment for separator glyphs.
Negative values lower the glyph, positive values raise it."
  :type 'float
  :group 'elline)

(defcustom elline-show-project t
  "If non-nil, show the current project or tab name in the mode-line."
  :type 'boolean
  :group 'elline)

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

(defun elline--color (key)
  "Return a theme-adaptive colour for KEY."
  (let* ((active   (elline--active-p))
         (ml-face  (if active 'mode-line 'mode-line-inactive))
         (bg       (or (face-attribute ml-face :background nil 'default) "#1e1e2e"))
         (fg       (or (face-attribute ml-face :foreground nil 'default) "#cdd6f4"))
         (accent   (or (face-attribute 'font-lock-keyword-face :foreground nil 'default) "#89b4fa"))
         (warning  (or (face-attribute 'warning  :foreground nil 'default) "#f9e2af"))
         (error-c  (or (face-attribute 'error    :foreground nil 'default) "#f38ba8"))
         (success  (or (face-attribute 'success  :foreground nil 'default) "#a6e3a1"))
         (blocks-p (eq elline-theme-style 'blocks)))
    (pcase key
      ('bg-main bg)
      ;; accent-tinted blocks to avoid the boring look
      ('bg-alt  (if blocks-p (elline--blend bg accent (if active 0.12 0.06)) bg))
      ('bg-alt2 (if blocks-p (elline--blend bg accent (if active 0.25 0.12)) bg))
      ('bg-neutral (if blocks-p (elline--blend bg fg (if active 0.10 0.05)) bg))
      ('fg      fg)
      ('accent  accent)
      ('warning warning)
      ('error   error-c)
      ('success success)
      ;; fg-dim is now readable but still de-emphasised
      ('fg-dim  (elline--blend fg bg (if active 0.55 0.65)))
      ('fg-inactive (elline--blend fg bg 0.60))
      (_ bg))))

;; Icon selection
;; -------------------------------------------------------------------------

(defun elline--icon (family name &optional fallback)
  "For each icon-provider, return an icon glyph for FAMILY with NAME.
Otherwise use FALLBACK."
  (let ((fallback (or fallback "")))
    (condition-case nil
        (pcase elline-icon-provider
          ('nerd-icons
           (pcase family
             ('oct     (nerd-icons-octicon  (concat "nf-oct-" name)))
             ('fa      (nerd-icons-faicon   (concat "nf-fa-"  name)))
             ('md      (nerd-icons-mdicon   (concat "nf-md-"  name)))
             ('codicon (nerd-icons-codicon  (concat "nf-cod-" name)))
             ('file    (or (nerd-icons-icon-for-file (or buffer-file-name (buffer-name)))
                           (nerd-icons-faicon "nf-fa-file")))
             ('mode    (or (nerd-icons-icon-for-mode major-mode)
                           (nerd-icons-codicon "nf-cod-code")))))
          ('all-the-icons
           (pcase family
             ('file (all-the-icons-icon-for-file (or buffer-file-name (buffer-name))))
             ('mode (all-the-icons-icon-for-mode major-mode))
             (_     (all-the-icons-faicon (or name "cog")))))
          (_ fallback))
      (error fallback))))

;; Segment & Separator logic
;; -------------------------------------------------------------------------

(defun elline--seg (text &optional bg fg icon bold)
  "Return a propertized mode-line segment with TEXT.
BG defaults to `bg-main`."
  (let* ((bg (or bg (elline--color 'bg-main)))
         (fg (or fg (elline--color 'fg)))
         (face `(:background ,bg :foreground ,fg ,@(when bold '(:weight bold)))))
    (propertize (concat " " (when icon (concat icon " ")) text " ") 'face face)))

(defun elline--get-bg (str default-bg)
  "Extract the DEFAULT-BG colour from the first character of STR."
  (if (and str (> (length str) 0))
      (let ((face (get-text-property 0 'face str)))
        (cond
         ((listp face) (or (plist-get face :background) default-bg))
         ((facep face) (face-attribute face :background nil 'default))
         (t default-bg)))
    default-bg))

(defun elline--join-left (segs default-bg)
  "Join SEGS with left-pointing separators."
  (let* ((blocks-p (eq elline-theme-style 'blocks))
         (sep-glyph (and blocks-p
                         (not (eq elline-separator-style 'none))
                         (car (alist-get elline-separator-style elline-separators))))
         (res "")
         (prev-bg default-bg))
    (dolist (seg segs)
      (when (and seg (not (string-empty-p seg)))
        (let ((cur-bg (elline--get-bg seg default-bg)))
          (when (and sep-glyph (not (string= prev-bg cur-bg)) (not (string= res "")))
            ;; Use a wrapper space + raise to bypass mode-line display stripping
            (let ((sep (propertize sep-glyph
                                   'face `(:foreground ,prev-bg :background ,cur-bg :height 0.95)
                                   'display `((raise . ,elline-separator-raise)))))
              (setq res (concat res sep))))
          (setq res (concat res seg))
          (setq prev-bg cur-bg))))
    ;; cap off the right edge of the left group
    (when (and sep-glyph (not (string= prev-bg default-bg)))
      (setq res (concat res (propertize sep-glyph
                                        'face `(:foreground ,prev-bg :background ,default-bg :height 0.95)
                                        'display `((raise . ,elline-separator-raise))))))
    res))

(defun elline--join-right (segs default-bg)
  "Join SEGS with right-pointing separators."
  (let* ((blocks-p (eq elline-theme-style 'blocks))
         (sep-glyph (and blocks-p
                         (not (eq elline-separator-style 'none))
                         (cdr (alist-get elline-separator-style elline-separators))))
         (res "")
         (left-bg default-bg))
    (dolist (seg segs)
      (when (and seg (not (string-empty-p seg)))
        (let ((right-bg (elline--get-bg seg default-bg)))
          (when (and sep-glyph (not (string= left-bg right-bg)))
            (setq res (concat res (propertize sep-glyph
                                              'face `(:foreground ,right-bg :background ,left-bg :height 0.95)
                                              'display `((raise . ,elline-separator-raise))))))
          (setq res (concat res seg))
          (setq left-bg right-bg))))
    res))

;; Segments
;; -------------------------------------------------------------------------
(defun elline--evil ()
  "Evil mode state pill."
  (when (bound-and-true-p evil-mode)
    (let* ((active   (elline--active-p))
           (blocks-p (eq elline-theme-style 'blocks))
           (state    (or evil-state 'normal))
           (icon     (pcase state
                       ('normal   (elline--icon 'oct "terminal" "⌨"))
                       ('insert   (elline--icon 'oct "plus"    "➕"))
                       ('visual   (elline--icon 'oct "eye"     "👁"))
                       ('replace  (elline--icon 'oct "sync"    "↻"))
                       ('operator (elline--icon 'oct "zap"     "⚡"))
                       ('motion   (elline--icon 'oct "arrow_right" "➡"))
                       ('emacs    (elline--icon 'oct "code"    "💻"))
                       (_         (elline--icon 'oct "question" "?"))))
           (state-c  (pcase state
                       ('normal   (elline--color 'accent))
                       ('insert   (elline--color 'success))
                       ('visual   (elline--color 'warning))
                       ('replace  (elline--color 'error))
                       ('operator (elline--color 'warning))
                       ('motion   (elline--color 'accent))
                       ('emacs    (elline--color 'fg-dim))
                       (_         (elline--color 'fg))))
           (bg       (if blocks-p
                         (if active state-c (elline--color 'bg-alt2))
                       (elline--blend (elline--color 'bg-main) "#000000" 0.10)))
           (fg       (if blocks-p
                         (if active (elline--color 'bg-main) (elline--color 'fg-dim))
                       state-c))
           ;; centering box
           (box-width 4)
           (content (string-trim (format " %s " icon)))  ; ensure single icon with minimal padding
           (len (length content))
           (pad-left (floor (/ (- box-width len) 2)))
           (pad-right (- box-width len pad-left))
           (pad-left (max 0 pad-left))
           (pad-right (max 0 pad-right))
           (boxed (concat (make-string pad-left ?\s) content (make-string pad-right ?\s))))
      (propertize boxed 'face `(:background ,bg :foreground ,fg :weight bold)))))


(defun elline--winum ()
  (when (and (fboundp 'winum-get-number) (> (window-width) 20))
    (let ((n (winum-get-number)))
      ;; bg-alt lets the accent icon pop without being overwhelming
      (when n (elline--seg (number-to-string n)
                           (elline--color 'bg-alt)
                           (elline--color 'accent))))))

(defun elline--status ()
  (let* ((ro     buffer-read-only)
         (mod    (and (buffer-modified-p) (not ro)))
         (narrow (buffer-narrowed-p))
         (icon   (cond (ro     (elline--icon 'oct "lock"        ""))
                       (mod    (elline--icon 'oct "dot_fill"    "●"))
                       (narrow (elline--icon 'oct "screen_full" "▼"))
                       (t      (elline--icon 'oct "dot"         "○"))))
         (fg     (cond (ro     (elline--color 'error))
                       (mod    (elline--color 'warning))
                       (t      (elline--color 'fg)))))
    (elline--seg "" (elline--color 'bg-alt) fg icon)))

(defun elline--buffer-name ()
  (let* ((name   (buffer-name))
         (remote (file-remote-p default-directory))
         (icon   (elline--icon 'file nil "")))
    (elline--seg (concat (when remote (concat (elline--icon 'oct "globe" "🌐") " ")) name)
                 (elline--color 'bg-alt2) (elline--color 'fg) icon t)))


;; Project/Tab awareness
;; ---------------------

(defun elline--frame-has-user-tabs-p ()
  "Return non-nil when the current frame has more than the built-in default tab."
  (let ((tabs (frame-parameter nil 'tabs)))
    (> (length (if (listp tabs) tabs nil)) 1)))

(defun elline--project ()
  "Show the current project name.
Adds the repo icon only when inside a tab."
  (when (and elline-show-project (> (window-width) 40))
    (let* ((name nil)
           ;; only fetch the icon if we are actually in a multi-tab workspace
           (icon (when (elline--frame-has-user-tabs-p)
                   (elline--icon 'oct "repo" "📁"))))
      ;; prefer project.el
      (when (fboundp 'project-current)
        (let ((proj (ignore-errors (project-current))))
          (when proj
            (setq name (if (fboundp 'project-name)
                           (project-name proj)
                         (file-name-nondirectory
                          (directory-file-name (project-root proj))))))))
      
      ;; fallback to tab-bar tab name
      (unless name
        (when (fboundp 'tab-bar-tab-name-current)
          (let ((tab-name (ignore-errors (tab-bar-tab-name-current))))
            (when (and tab-name (not (string= tab-name (buffer-name))))
              (setq name tab-name)))))
      
      (when name
        (elline--seg name
                     (elline--color 'bg-alt)
                     (elline--color 'accent)
                     icon)))))

(defun elline--git ()
  (when (and vc-mode (stringp vc-mode))
    (let ((backend (ignore-errors (vc-backend buffer-file-name))))
      (when (eq backend 'Git)
        (let* ((branch (replace-regexp-in-string "^ Git[:-]?\\s-*" "" vc-mode))
               (dirty  (and buffer-file-name (fboundp 'vc-git-state) (not (eq 'up-to-date (vc-git-state buffer-file-name)))))
               (icon   (elline--icon 'oct "git_branch" "")))
          (elline--seg branch (elline--color 'bg-alt)
                       ;; Full fg for clean branches; warning pops for dirty
                       (if dirty (elline--color 'warning) (elline--color 'fg)) icon))))))

(defun elline--macro ()
  (when defining-kbd-macro
    ;; Theme-adaptive foreground instead of harsh #ffffff
    (elline--seg "REC" (elline--color 'error)
                 (elline--color 'bg-main)
                 (elline--icon 'fa "circle" "●") t)))

(defun elline--selection ()
  (when (and (use-region-p) (> (window-width) 80))
    ;; Use fg here instead of accent so it's readable on bg-alt2
    (elline--seg (format "%dL %dC" (count-lines (region-beginning) (region-end)) (abs (- (region-end) (region-beginning))))
                 (elline--color 'bg-alt) (elline--color 'fg))))

(defun elline--major-mode ()
  (let* ((name (capitalize (string-trim (symbol-name major-mode) nil "-mode$")))
         (icon (elline--icon 'mode nil "")))
    (elline--seg name (elline--color 'bg-alt) (elline--color 'fg) icon)))

(defun elline--process ()
  (when mode-line-process
    (let ((str (format-mode-line mode-line-process)))
      (unless (string-blank-p str)
        (elline--seg str (elline--color 'bg-alt2) (elline--color 'accent))))))

(defun elline--lsp ()
  (when (> (window-width) 60)
    (when (or (bound-and-true-p lsp-mode) (bound-and-true-p eglot--managed-mode))
      (elline--seg "LSP" (elline--color 'bg-alt) (elline--color 'success)
                   (elline--icon 'codicon "symbol_method" "λ")))))

(defun elline--flycheck ()
  (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-count-errors) (> (window-width) 50))
    (let* ((counts (when flycheck-current-errors (flycheck-count-errors flycheck-current-errors)))
           (e (or (alist-get 'error counts) 0))
           (w (or (alist-get 'warning counts) 0))
           (i (or (alist-get 'info counts) 0))
           (bg (elline--color 'bg-alt))) ;; bg-alt to continue the block
      (concat
       (propertize (format " %s %d" (elline--icon 'oct "x_circle" "✖") e) 'face `(:background ,bg :foreground ,(if (> e 0) (elline--color 'error) (elline--color 'fg-dim)) :weight bold))
       (propertize (format " %s %d" (elline--icon 'oct "alert" "⚠") w)    'face `(:background ,bg :foreground ,(if (> w 0) (elline--color 'warning) (elline--color 'fg-dim)) :weight bold))
       (propertize (format " %s %d " (elline--icon 'oct "info" "ℹ") i)    'face `(:background ,bg :foreground ,(if (> i 0) (elline--color 'success) (elline--color 'fg-dim)) :weight bold))))))

(defun elline--flymake-count (type)
  "Robustly count Flymake diagnostics matching TYPE ('error, 'warning, 'note)."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (let* ((diag-type (flymake-diagnostic-type d))
             ;; backends like Eglot use derived types 'eglot-error
             (category (or (get diag-type 'flymake-category) diag-type)))
        (when (or (eq category (pcase type
                                 ('error   'flymake-error)
                                 ('warning 'flymake-warning)
                                 ('note    'flymake-note)))
                  ;; fallback to older backend that use raw keywords
                  (eq diag-type (pcase type
                                  ('error   :error)
                                  ('warning :warning)
                                  ('note    :note))))
          (cl-incf count))))
    count))

(defun elline--flymake ()
  (when (and (bound-and-true-p flymake-mode) (> (window-width) 50))
    (let* ((e (elline--flymake-count 'error))
           (w (elline--flymake-count 'warning))
           (i (elline--flymake-count 'note))
           (bg (elline--color 'bg-alt)))
      (concat
       (propertize (format " %s %d" (elline--icon 'oct "x_circle" "✖") e)
                   'face `(:background ,bg :foreground ,(if (> e 0) (elline--color 'error) (elline--color 'fg-dim)) :weight bold))
       (propertize (format " %s %d" (elline--icon 'oct "alert" "⚠") w)
                   'face `(:background ,bg :foreground ,(if (> w 0) (elline--color 'warning) (elline--color 'fg-dim)) :weight bold))
       (propertize (format " %s %d " (elline--icon 'oct "info" "ℹ") i)
                   'face `(:background ,bg :foreground ,(if (> i 0) (elline--color 'success) (elline--color 'fg-dim)) :weight bold))))))

(defun elline--encoding ()
  (when (and buffer-file-coding-system (> (window-width) 40))
    (let* ((sys      (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol      (pcase eol-type (0 "LF") (1 "CRLF") (2 "CR") (_ "??")))
           (coding   (cond ((string-match-p "utf-8" sys) "UTF-8") ((string-match-p "latin-1" sys) "LATIN-1") (t (upcase sys)))))
      ;; Alternating block background, full fg for readability
      (elline--seg (format "%s %s" coding eol) (elline--color 'bg-alt2) (elline--color 'fg)))))

(defun elline--position ()
  (elline--seg (format "%d:%d" (line-number-at-pos) (current-column)) (elline--color 'bg-alt) (elline--color 'fg)))

(defun elline--percentage ()
  (when (> (window-width) 50)
    (elline--seg (format-mode-line "%p") (elline--color 'bg-alt2) (elline--color 'fg-dim))))

(defun elline--buffer-size ()
  (when (> (window-width) 60)
    (let ((size (buffer-size)))
      (elline--seg (cond ((> size 1048576) (format "%.1fM" (/ size 1048576.0))) ((> size 1024) (format "%.1fk" (/ size 1024.0))) (t (format "%dB" size)))
                   (elline--color 'bg-alt) (elline--color 'fg-dim)))))

(defun elline--time ()
  (when (and elline-show-time (> (window-width) 70))
    (elline--seg (format-time-string "%H:%M") (elline--color 'bg-alt2) (elline--color 'accent) (elline--icon 'oct "clock" "⏱"))))

;; Zones
;; -------------------------------------------------------------------------

(defun elline--active-bar ()
  (when (and (elline--active-p) (eq elline-theme-style 'flat))
    (propertize "▌"
                'face `(:foreground ,(elline--color 'accent)
                                    :background ,(elline--color 'bg-main)))))

(defun elline--build-left (default-bg w)
  (let ((responsive-p (not elline-responsive)))
    (elline--join-left
     (append
      (list (elline--active-bar)
            (elline--evil)
            (elline--winum)
            (elline--status)
            (elline--buffer-name)
            (elline--macro)
            (elline--selection))
      (when (or responsive-p (> w 50))
        (list (elline--major-mode)
              (elline--git)
              (elline--project)
              (elline--process))))
     default-bg)))

(defun elline--build-right (default-bg w)
  (let ((responsive-p (not elline-responsive)))
    (elline--join-right
     (when (or responsive-p (> w 30))
       (list (elline--lsp)
             (elline--flycheck)
             (elline--flymake)
             (elline--encoding)
             (elline--position)
             (elline--percentage)
             (elline--buffer-size)
             (elline--time)))
     default-bg)))

;; Main setup
;; -------------------------------------------------------------------------

(defun elline--build ()
  (let* ((w            (window-width))
         (default-bg   (elline--color 'bg-main))
         (left         (elline--build-left default-bg w))
         (right        (elline--build-right default-bg w))
         (right-width  (if right (string-width (format-mode-line right)) 0)))
    (list left
          `(:propertize " " display ((space :align-to (- right right-fringe right-margin ,right-width))))
          right)))

(defun elline-toggle-project ()
  "Toggle display of the project name in the mode-line."
  (interactive)
  (setq elline-show-project (not elline-show-project))
  (message "Modeline project: %s" (if elline-show-project "on" "off"))
  (force-mode-line-update t))

(defun elline--apply-height ()
  "Apply `elline-height` to modeline faces."
  (set-face-attribute 'mode-line nil :height elline-height)
  (set-face-attribute 'mode-line-inactive nil :height elline-height)
  (force-mode-line-update t))

(defun elline-cycle-icons ()
  (interactive)
  (setq elline-icon-provider (pcase elline-icon-provider ('nerd-icons 'all-the-icons) ('all-the-icons 'none) ('none 'nerd-icons)))
  (message "Modeline icons: %s" elline-icon-provider)
  (force-mode-line-update t))

(defun elline-toggle-time ()
  (interactive)
  (setq elline-show-time (not elline-show-time))
  (message "Modeline clock: %s" (if elline-show-time "on" "off"))
  (force-mode-line-update t))

(defun elline-toggle-style ()
  "Toggle between flat and blocks theme styles."
  (interactive)
  (setq elline-theme-style
        (if (eq elline-theme-style 'flat) 'blocks 'flat))
  (message "Modeline style: %s" elline-theme-style)
  (force-mode-line-update t))

(defun elline-cycle-separators ()
  "Cycle through separator styles. (Requires 'blocks' style active)."
  (interactive)
  (setq elline-separator-style
        (pcase elline-separator-style
          ('none  'arrow)
          ('arrow 'curve)
          ('curve 'slant)
          ('slant 'none)))
  (message "Modeline separators: %s" elline-separator-style)
  (unless (eq elline-theme-style 'blocks)
    (message "Note: Separators are only visible when style is set to 'blocks' (C-c m b)."))
  (force-mode-line-update t))

(defun elline--refresh-all-buffers ()
  (dolist (buf (buffer-list)) (with-current-buffer buf (force-mode-line-update)))
  (redisplay t))

;;;###autoload
(define-minor-mode elline-mode
  "Toggle the beautiful modeline globally."
  :global t
  :lighter nil
  (if elline-mode
      (progn
        (setq-default mode-line-format '("%e" (:eval (elline--build))))
        (elline--apply-height)
        (elline--refresh-all-buffers))
    (progn
      (setq-default mode-line-format (default-value 'mode-line-format))
      (elline--refresh-all-buffers))))

;; Keybindings
;; -------------------------------------------------------------------------

(global-set-key (kbd "C-c m i") #'elline-cycle-icons)
(global-set-key (kbd "C-c m t") #'elline-toggle-time)
(global-set-key (kbd "C-c m s") #'elline-cycle-separators)
(global-set-key (kbd "C-c m b") #'elline-toggle-style)
(global-set-key (kbd "C-c m p") #'elline-toggle-project)

(provide 'elline)
;;; elline.el ends here
