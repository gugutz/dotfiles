;;; Package --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal and highly customized configuration, with optmization based on doom-emacs and other stuff i found on the internet

;;; Code:

(when (version< emacs-version "26.1")
  (error "Detected Emacs %s.  This Emacs config only supports Emacs 26.1 and higher"
    emacs-version))

;; some usefull constants used throughout the config
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))


;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "init.elc" user-emacs-directory))
    (load-file (expand-file-name "init.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    ;;(require 'org)
    ;;(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))
    )
  )



;;****************************************************************
;;
;; Garbage Collection GC Settings

;; source: https://raw.githubusercontent.com/gilbertw1/emacs-literate-starter/master/emacs.org

(defvar custom-gc-cons-threshold 402653184)

;; increase to defer gc on emacs startup
(setq gc-cons-threshold most-positive-fixnum ;; 2^61 bytes
  gc-cons-percentage 0.6
  garbage-collection-messages t)


;; reset gc after emacs loads
;; 100mb is the value recommended by lsp github page
(eval-and-compile
  (add-hook 'emacs-startup-hook
    (setq gc-cons-threshold custom-gc-cons-threshold ;; 400mb
      gc-cons-percentage 0.6)))

;; automatically GC when emacs is out of focus
;; source: M-EMACS
(add-hook 'emacs-startup-hook
  (lambda ()
    (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
        (lambda ()
          (unless (frame-focus-state)
            (garbage-collect))))
      (add-hook 'after-focus-change-function 'garbage-collect))))


;; raise gc-cons-threshold while the minibuffer is active, so the GC doesn’t slow down expensive commands (or completion frameworks, like helm and ivy).

(defun gc-minibuffer-setup-hook ()
  "Set gc consing treshold to the higher possible value."
  (setq gc-cons-threshold most-positive-fixnum))


(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)

(defun gc-minibuffer-exit-hook ()
  "Set gc consing treshold to the higher possible value."
  ;; Defer it so that commands launched immediately after will enjoy the benefits.
  (run-at-time
    1 nil (lambda () (setq gc-cons-threshold custom-gc-cons-threshold))))

(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;; here is how to multiply gc-cons-treshold
;; (* better-gc-cons-threshold 2)

;;------------------------------------------

;; Unset file-name-handler-alist temporarily.
;; Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but during startup, it won’t need any of them.

(defvar original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist original-file-name-handler-alist)))

;;****************************************************************
;;
;;; BOOTSTRAP PACKAGE MANAGEMENT


;; put all load-paths into one single big file
(when EMACS27+
  (setq package-quickstart t))


;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(eval-and-compile
  (setq load-prefer-newer t
    package-user-dir (concat user-emacs-directory "elpa/")
    ;; don't try to mount the load path based on installed packages
    package-init-file-ensured t
    ;; don't load packages on startup; use-package will to that
    package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))
;; -------------------------------------


;; Manually Set `load-path'
;; This load-path will actually be faster than the one created by `package-initialize' because it appends the elpa packages to the end of the load path.

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

;; -------------------------------------

;; Initialize Package Management
;;
;; Using `eval-when-compile' to perform all of the package initialization during compilation so that when byte compiled, all of this time consuming code is skipped.
;; This can be done because the result of byte compiling `use-package' statements results in the macro being fully expanded at which point =use-package= isn't actually required any longer.

;; Since the code is automatically compiled during runtime, if the configuration hasn't already been previously compiled manually then all of the package initialization will still take place at startup.

;; use-package settings
(setq use-package-always-defer t)
(setq use-package-verbose t)

(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  ;; emacs 27 calls package initialize automatically so `package-initilize' is only required on older versions
  (package-initialize)

  ;; install use-package if not already installed
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; load use-package
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-compute-statistics t)
  )


;;****************************************************************
;;
;;; Optimizations from doom emacs

;;; Source: https://github.com/hlissner/doom-emacs/blob/develop/core/core.el#L228

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right
  bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; defer fontification while there is input pending.
(setq jit-lock-defer-time 0)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)


;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))



;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later.
(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (defun tau-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;;****************************************************************
;;
;;; EMACS CORE EDITOR SETTINGS

(setq ring-bell-function 'ignore) ;; Disable the annoying Emacs bell ring (beep)


;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

(setq kill-do-not-save-duplicates t) ;; Eliminate duplicates in the kill ring.

(setq next-line-add-newlines t) ;; C-n insert newlines if the point is at the end of the buffer.

;; **************************************
;; package image

(setq image-animate-loop t)

;;****************************************************************
;;
;;; UI SETTINGS AND APPEARANCE


;; **************************************
;;; Theme & font

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; Add `./themes' to path
(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")

;; Set the used theme
(setq my-theme 'vscode-dark-plus)

;;  Load the theme
;; (add-hook 'window-setup-hook (lambda () (load-theme my-theme t)))
(load-theme my-theme t)

;;****************************************************************
;;
;;; SCROLLING SETTINGS

;; trying to solve emacs garbage collecting and redisplaying a lot while fast scrolling
;;(setq redisplay-dont-pause t)

(setq hscroll-margin 2
  hscroll-step 1
  ;; Emacs spends too much effort recentering the screen if you scroll the
  ;; cursor more than N lines past window edges (where N is the settings of
  ;; `scroll-conservatively'). This is especially slow in larger files
  ;; during large-scale scrolling commands. If kept over 100, the window is
  ;; never automatically recentered.
  scroll-conservatively 101
  scroll-margin 0
  scroll-preserve-screen-position t
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  auto-window-vscroll nil)


;; mouse
(setq mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling
(setq mouse-wheel-scroll-amount '(3 ((control) . 6))) ;; hold Control for 5 lines at a time
(setq mouse-wheel-scroll-amount '(5 ((shift) . 2)))


;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(add-hook 'eshell-mode-hook (lambda () (hscroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (hscroll-margin 0)))


;;****************************************************************
;;
;;; CURSOR SETTINGS

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;****************************************************************
;;
;;; FRINGES

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
  indicate-empty-lines nil)

;; remove continuation arrow on right fringe (find out why this isnt working)
;; (delq 'fringe-indicator-alist 'assq)
;; (delq 'continuation 'assq)

;; indicate empty lines in the fringe with --
(setq-default indicate-empty-lines t)


;;****************************************************************
;;
;;; Window and Frames

;; A simple frame title
(setq frame-title-format '("%b – Tau Emacs")
  icon-title-format frame-title-format)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
  frame-resize-pixelwise t)

(unless (assq 'menu-bar-lines default-frame-alist)
  ;; We do this in early-init.el too, but in case the user is on Emacs 26 we do
  ;; it here too: disable tool and scrollbars, as Doom encourages
  ;; keyboard-centric workflows, so these are just clutter (the scrollbar also
  ;; impacts performance).
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))


;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Prompt the user for confirmation when deleting a non-empty frame
(global-set-key [remap delete-frame] #'doom/delete-frame)

;; always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
  split-height-threshold nil)


;; disable scroll bars from frames
(scroll-bar-mode -1)

;; Remove the menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil))


;; auto balance windows on opening and closing frames
(setq window-combination-resize t)

;; ** auto balance windows area
(global-set-key (kbd "C-M-+") 'balance-windows-area)


;; *********************************
;; Frame transparency toggle

(defun tau/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                 ((numberp (cdr alpha)) (cdr alpha))
                 ;; Also handle undocumented (<active> <inactive>) form.
                 ((numberp (cadr alpha)) (cadr alpha)))
            100)
        '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'tau/toggle-transparency)

;; *********************************
;; Eyebrowse

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  )

;; *********************************
;; Ace-Window

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :config
  (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; set the window labels in the home row
  )

;; *********************************
;; emacsrotate

(use-package rotate
  :bind
  ("C-c r w" . rotate-window)
  ("C-c r l" . rotate-layout)
  ("M-S-O SPC" . rotate-layout)
  ("C-M-o" . hydra-frame-window/body)
  )



;; *********************************
;; ** windmove

;; (use-package windmove
;;   :config
;;   ;; use shift + arrow keys to switch between visible buffers
;;   ;; (windmove-default-keybindings)
;;   )

;; *********************************
;; Highlight lines

(use-package hl-line
  :ensure nil
  :preface
  (defun tau-disable-hl-line-h ()
    (when hl-line-mode
      (setq-local tau-buffer-hl-line-mode t)
      (hl-line-mode -1)))

  (defun tau-enable-hl-line-maybe-h ()
    (when tau-buffer-hl-line-mode
      (hl-line-mode +1)))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar tau-buffer-hl-line-mode nil)
  :hook
  ((prog-mode text-mode conf-mode) . hl-line-mode)
  ((evil-visual-state-entry-hook activate-mark-hook) . tau-disable-hl-line-h)
  ((evil-visual-state-exit-hook deactivate-mark-hook) . tau-enable-hl-line-maybe-h)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
    global-hl-line-sticky-flag nil)

  )


;; *********************************
;; highlight indent guides

(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode toml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character) ; column
  )


;; **************************************
;; show paren mode

;; Highlight (by bolding) the matching parenthesis

(use-package paren
  :ensure nil
  :defer 2
  :custom-face
  (show-paren-match ((nil (:background "#9370DB" :foreground "#ffffff" :weight bold :box t)))) ;; :box t
  (show-paren-mismatch ((nil (:background "red" :foreground "black")))) ;; :box t
  :init
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode +1)
  )


;; **************************************
;; Highlighting numbers

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")
  )


;; **************************************
;; Highlighting operators

(use-package highlight-operators
  :hook
  (prog-mode . highlight-operators-mode)
  )

;; *********************************
;; ** Highlighting escape sequences


(use-package highlight-escape-sequences
  :hook
  (prog-mode . hes-mode)
  )

;; **************************************
;;; package rainbow-delimiters

;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(setq rainbow-delimiters-max-face-count 3)

;; **************************************
;;; package pos-tip
(setq pos-tip-internal-border-width 6
  pos-tip-border-width 1)


;; **************************************
;; display line numbers

(use-package display-line-numbers
  :ensure nil
  :init
  ;; Explicitly define a width to reduce computation
  (setq-default display-line-numbers-width 3)
  ;; Show absolute line numbers for narrowed regions makes it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  )


;; *********************************
;; emacs 27 native tabs

(use-package tabbar
  :if (> emacs-major-version 26)
  :ensure nil
  :hook
  (window-setup . tab-bar-mode) ;; per-frame
  ;; (window-setup . tab-line-mode +1) ;; per window
  )



;;****************************************************************
;;
;;; Buffers settings

;; **************************************
;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.

(push '(buffer-predicate) default-frame-alist)
(setq confirm-nonexistent-file-or-buffer t)

;; **************************************
;; C-k kills current buffer without having to select which buffer

;; prompt only if there are unsaved changes.
(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  )

(global-set-key (kbd "C-x k") #'kill-current-buffer)

;; **************************************
;; refresh buffer with F5
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))


;;****************************************************************
;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
  ;; But don't let the minibuffer grow beyond this size
  max-mini-window-height 0.15)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)




;; *********************************
;; Highlighting parentheses

;; This mode highlights (coloring) the current pair in which the point (cursor) is

(use-package highlight-parentheses
  :diminish
  :hook
  (prog-mode . highlight-parentheses-mode)
  :init
  (setq hl-paren-colors '("firebrick1" "IndianRed1" "IndianRed3" "IndianRed4"))
  (setq hl-paren-background-colors '("#eee222" "#ccba85" "#bceae7" "#2aa020"))
  )

;; *********************************
;; Rainbow Delimiters


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3)
  )

;; *********************************
;; Rainbow Blocks


(use-package rainbow-blocks
  :hook
  (prog-mode . rainbow-blocks-mode)
  )

;; *********************************
;; ** Highlight TODO

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  :config
  ;; Adding a new keyword: TEST.
  (setq hl-todo-keyword-faces
    '(("TODO"   . "#FF3300")
       ("FIXME"  . "#FF0000")
       ("DEBUG"  . "#A020F0")
       ("GOTCHA" . "#FF4500")
       ("NOTE"   . "#ffff00")
       ("DONE"   . "#00ff00")
       ("STUB"   . "#1E90FF")))
  )


;; *********************************
;; rainbow mode

;; : Colorize hex, rgb and named color codes

(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode)
  (web-mode . rainbow-mode)
  (elisp-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode)
  (conf-mode . rainbow-mode)
  )



;;****************************************************************
;;
;;; Run all types of escapes when pressing C-g or ESC
(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun tau/escape ()
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
          ;; quit the minibuffer if open.
          (abort-recursive-edit))
    ;; Run all escape hooks. If any returns non-nil, then stop there.
    ((run-hook-with-args-until-success 'doom-escape-hook))
    ;; don't abort macros
    ((or defining-kbd-macro executing-kbd-macro) nil)
    ;; Back to the default
    ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'tau/escape)


(use-package diminish)

;;****************************************************************
;;
;;; Startup

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
          (time-subtract after-init-time before-init-time)))
      gcs-done)))


;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; start with init.el
(setq initial-buffer-choice "~/dotfiles/emacs.d/init.el")
;;(setq initial-buffer-choice nil)
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list")


;;****************************************************************
;;
;;; Folders and Files

;;; Define emacs directory
(defconst emacs-dir (file-truename user-emacs-directory))

;; Define a config folder inside emacs-dir
(defconst config-dir (concat emacs-dir "config/"))

;; Define a config folder inside emacs-dir
(defconst cache-dir (concat emacs-dir "cache/"))

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t)

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; disable emacs's automatic backup~ file
(setq make-backup-files nil)

;; dont ask confirmation to kill processes
(setq confirm-kill-processes nil)

(setq vc-follow-symlinks t)

;; Prevent emacs to create lockfiles (.#files#)
(setq create-lockfiles nil)


;; Prevent emacs from writing several files in the config folder
(setq async-byte-compile-log-file  (concat cache-dir "async-bytecomp.log"))
(setq bookmark-default-file        (concat config-dir "bookmarks"))
(setq custom-file                  (concat config-dir "custom.el"))
(setq url-cache-directory          (concat config-dir "url/"))
(setq url-configuration-directory  (concat config-dir "url/"))


(use-package abbrev
  :defer 3
  :ensure nil
  :config
  (setq abbrev-file-name (concat config-dir "abbrev.el"))
  )



(use-package recentf
  :ensure nil
  ;; Loads after 1 second of idle time.
  :defer 2
  :init
  :hook
  (pre-command . recentf-mode)
  :config
  (setq recentf-save-file "~/.emacs.d/config/recentf")
  (setq recentf-max-menu-items 0)
  (setq recentf-max-saved-items 300)
  (setq recentf-filename-handlers '(file-truename))
  (setq recentf-exclude
    (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
      "^/var/folders/.+$"
      ;; ignore private DOOM temp files (but not all of them)
      (concat "^" (file-truename user-emacs-directory))))
  )

(use-package uniquify
  :ensure nil
  ;; Less important than recentf.
  :defer 2
  )

(use-package server
  :ensure nil
  ;; Less important than recentf.
  :defer 5
  )

(use-package tramp
  :defer 5
  :ensure nil
  :config
  (setq tramp-auto-save-directory    (concat config-dir "tramp-auto-save/"))
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-persistency-file-name  (concat config-dir "tramp-persistency.el"))
  )

;; **********************************
;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)


;;****************************************************************
;;
;;; MINIBUFFER SETTINGS

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; But don't let the minibuffer grow beyond this size
(setq max-mini-window-height 0.15)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)



;;****************************************************************
;;
;;; INDENTATION SETTINS

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)
;; default indent to 2 spaces
(setq-default tab-width 2)

;; make tab key always call a indent command.
(setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; Perl default indent size
(setq-default cperl-basic-offset 2)
;; C e C-like langs default indent size
(setq-default c-basic-offset 2)
;; Lisp languages
(setq-default lisp-basic-offset 2)

;; make return key also do indent, for current buffer only
;; (electric-indent-local-mode 1)

;; make return key also do indent, globally
(electric-indent-mode 1)

;;****************************************************************
;;
;;; Evil

(use-package evil
  :demand t
  :config
  (evil-mode 1)
  ;; change cursor color according to mode
  ;; (setq evil-emacs-state-cursor '("#ff0000" box))
  ;; (setq evil-motion-state-cursor '("#FFFFFF" box))
  ;; (setq evil-normal-state-cursor '("#00ff00" box))
  ;; (setq evil-visual-state-cursor '("#abcdef" box))
  ;; (setq evil-insert-state-cursor '("#e2f00f" bar))
  ;; (setq evil-replace-state-cursor '("#ff0000" hbar))
  ;; (setq evil-operator-state-cursor '("#ff0000" hollow))
  ;; evil in modeline
  (setq evil-mode-line-format '(before . mode-line-front-space)) ;; move evil tag to beginning of modeline
  ;;; FOR SOME REASON WHEN I MAP KEYS IN :bind EVIL DOENST LOAD ON STARTUP
  (define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
  ;; ("SPC q" . kill-buffer-and-window) ;; check if evil quit does this
  (define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC d") 'delete-frame)
  (define-key evil-normal-state-map (kbd "SPC k") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "SPC 0") 'delete-window)
  (define-key evil-normal-state-map (kbd "SPC -") 'split-window-bellow)
  (define-key evil-normal-state-map (kbd "SPC |") 'split-window-right)
  (define-key evil-normal-state-map (kbd "SPC u") 'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "SPC o") 'other-window)
  (define-key evil-normal-state-map (kbd "h") 'backward-char)
  (define-key evil-normal-state-map (kbd "j") 'next-line)
  (define-key evil-normal-state-map (kbd "k") 'previous-line)
  (define-key evil-normal-state-map (kbd "l") 'forward-char)

  ;; ------------------------------------------
  ;;
  ;;; RECOVER SOME USEFUL EMACS NATIVE COMMANDS
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "M-w") 'kill-ring-save)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-normal-state-map (kbd "C-y") 'yank)

  ;; remap native find definition (evil overrides it with some useless function)
  (global-set-key (kbd "M-.") 'xref-find-definitions)
  ;; somehow global-set-key doenst work for normal mode for this binding, so i manually set it
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

  (global-set-key (kbd "C-S-H") 'evil-window-left)
  (global-set-key (kbd "C-S-L") 'evil-window-right)
  (global-set-key (kbd "C-S-K") 'evil-window-up)
  (global-set-key (kbd "C-S-J") 'evil-window-down)
  )


;; *********************************
;; undo tree

;; Branching & persistent undo
(use-package undo-tree
  :hook
  ;; There is no hook specifically for switching buffers, but you can use the buffer-list-update-hook . Switching buffers re-orders the buffer list and triggers this hook, so you could use that to run a command that checks the major-mode of the current buffer and takes some action if it is eshell.
  ;; TODO: check if this gets activated constantly
  (buffer-list-update . (lambda () (global-undo-tree-mode +1)))
  (find-file . (lambda () (global-undo-tree-mode +1)))
  :config
  (setq undo-tree-visualizer-diff t
    undo-tree-auto-save-history t
    undo-tree-enable-undo-in-region t
    ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
    ;; truncating the undo history and corrupting the tree. See
    ;; https://github.com/syl20bnr/spacemacs/issues/12110
    undo-limit 800000
    undo-strong-limit 12000000
    undo-outer-limit 120000000
    undo-tree-history-directory-alist
    `(("." . ,(concat cache-dir "undo-tree-hist/")))
    )
  ;;(global-undo-tree-mode +1)

  )


;; *********************************
;; so long

;; this package was added to emacs 27

(if EMACS27+
  (global-so-long-mode 1)
  (use-package so-long
    :config
    (global-so-long-mode 1)
    )
  )

;; *********************************
;; evil-commentary

(use-package evil-commentary
  :diminish
  :hook
  (evil-mode . evil-commentary-mode)
  :init
  (evil-commentary-mode)
  )

;; *********************************
;; evil-surround
(use-package evil-matchit
  :disabled
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

;; *********************************
;; evil-surround

(use-package evil-surround
  :disabled
  :preface
  (defun evil-surround-prog-mode-hook-setup ()
    "Add more pairs to prog mode"
    (push '(47 . ("/" . "/")) evil-surround-pairs-alist)
    (push '(40 . ("(" . ")")) evil-surround-pairs-alist)
    (push '(41 . ("(" . ")")) evil-surround-pairs-alist)
    (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
    (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
    )
  (defun evil-surround-js-mode-hook-setup ()
    "ES6." ;  this is a documentation string, a feature in Lisp
    ;; I believe this is for auto closing pairs
    (push '(?1 . ("{`" . "`}")) evil-surround-pairs-alist)
    (push '(?2 . ("${" . "}")) evil-surround-pairs-alist)
    (push '(?4 . ("(e) => " . "(e)")) evil-surround-pairs-alist)
    ;; ReactJS
    (push '(?3 . ("classNames(" . ")")) evil-surround-pairs-alist)
    )
  (defun evil-surround-emacs-lisp-mode-hook-setup ()
    (push '(?` . ("`" . "'")) evil-surround-pairs-alist)
    )
  (defun evil-surround-org-mode-hook-setup ()
    (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
    (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
    (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
    )
  :hook
  (prog-mode-hook . evil-surround-prog-mode-hook-setup)
  (js2-mode-hook . evil-surround-js-mode-hook-setup)
  (emacs-lisp-mode-hook . evil-surround-emacs-lisp-mode-hook-setup)
  (org-mode-hook . evil-surround-org-mode-hook-setup)
  :config
  (global-evil-surround-mode 1)
  )

;;****************************************************************
;;
;;; Ivy

(use-package ivy
  :demand t
  :bind
  ("C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g t" . counsel-git)
  ("C-c g g" . counsel-git-grep)
  ("C-c a g" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  :config
  (ivy-mode 1)
  ;; Add recent files and bookmarks to the ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;;Displays the current and total number in the collection in the prompt
  (setq enable-recursive-minibuffers t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  ;; ;; remove the regex anchor ^ from `counsel-M-x' to match any substring, not only the ones that begin with the input
  ;; (setq ivy-initial-inputs-alist nil)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC b") 'counsel-switch-buffer)
    (define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)
    (define-key evil-normal-state-map (kbd "SPC f z") 'counsel-fzf)
    (define-key evil-normal-state-map (kbd "SPC p f") 'counsel-projectile-find-file)
    (define-key evil-normal-state-map (kbd "SPC a g") 'counsel-ag)
    (define-key evil-normal-state-map (kbd "SPC r g") 'counsel-rg)
    )
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  )


;; *********************************
;;; Counsel

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :hook
  (ivy-mode . counsel-mode)
  :config
  (eval-when-compile
    (counsel-mode))
  )

;; ** Counsel integration for Projectile

(use-package counsel-projectile
  :after ivy counsel projectile
  :config
  (eval-when-compile
    (counsel-projectile-mode 1))
  )

;; *********************************
;; Enhance M-x

;; Enhancement for `execute-extended-command'. Auto detects and uses ivy or ido, if installed
(use-package amx
  :after ivy
  :config
  (setq amx-save-file "~/.emacs.d/config/amx-items")
  )

;; *********************************
;; fuzzy matching

;; Ivy's `ivy--regex-fuzzy' function uses flx automatically if its installed
(use-package flx
  :disabled
  :after ivy
  :config
  ;; use fuzzy matching everywhere except for swiper
  (with-eval-after-load 'ivy
    (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
      ivy-re-builders-alist)
    (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))
  )
;; *********************************
;; ivy-rich

(use-package ivy-rich
  :hook
  (ivy-mode . ivy-rich-mode)
  :preface
  ;; use all-the-icons for `ivy-switch-buffer'
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
      (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  :init
  ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  (setq ivy-rich-path-style 'abbrev)

  (setq ivy-rich-display-transformers-list
    '(ivy-switch-buffer
       (:columns
         ((ivy-rich-switch-buffer-icon (:width 3))
           (ivy-rich-candidate (:width 30))
           (ivy-rich-switch-buffer-size (:width 7))
           (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
           (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
           (ivy-rich-switch-buffer-project (:width 15 :face success))
           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
       counsel-find-file
       (:columns
         ((ivy-read-file-transformer)
           (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
       counsel-M-x
       (:columns
         ((counsel-M-x-transformer (:width 35))
           (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
       counsel-projectile-switch-project
       (:columns
         ((ivy-rich-counsel-projectile-switch-project-project-name (:width 20 :face success))
           (ivy-rich-candidate)))
       counsel-describe-function
       (:columns
         ((counsel-describe-function-transformer (:width 35))
           (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
         ((counsel-describe-variable-transformer (:width 35))
           (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
       package-install
       (:columns
         ((ivy-rich-candidate (:width 25))
           (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
           (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
           (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))
       counsel-projectile-find-file
       (:columns
         ((ivy-rich-switch-buffer-icon (:width 2))
           (ivy-rich-candidate (:width 30))
           (ivy-rich-switch-buffer-size (:width 7))
           (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
           (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
           (ivy-rich-switch-buffer-project (:width 15 :face success))
           (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
       )
    )
  ;; ivy-rich-mode needs to be called after `ivy-rich--display-transformers-list' is changed
  :config
  (with-eval-after-load 'ivy
    (ivy-rich-mode 1))
  )

;; *********************************
;; ivy-posframe

;; Requires: Emacs >= 26

(use-package ivy-posframe
  :after ivy counsel
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe ((t (:background "#202020"))))
  (ivy-posframe-border ((t (:background "#9370DB"))))
  (ivy-posframe-cursor ((t (:background "#00ff00"))))
  :config
  (eval-when-compile
    (setq ivy-posframe-width 130)
    (setq ivy-posframe-internal-border-width 3)
    (setq ivy-posframe-parameters
      '(
         (left-fringe . 8)
         (right-fringe . 8)
         (internal-border-width . 1)
         (alpha . 85)
         ))
    ;; define the position of the posframe per function
    (setq ivy-posframe-display-functions-alist
      '(
         (counsel-M-x     . nil)
         (swiper          . nil)
         (complete-symbol . ivy-posframe-display-at-point)
         (t               . ivy-posframe-display-at-frame-center)
         ))
    ;; custom define height of post frame per function
    (setq ivy-posframe-height-alist
      '(
         (find-file . 15)
         (counsel-find-file . 15)
         (counsel-projectile-find-file . 15)
         (switch-buffer . 15)
         (counsel-switch-buffer . 15)
         (counsel-ag . 15)
         (counsel-projectile-ag . 30)
         (counsel-rg . 15)
         (counsel-projectile-rg . 30)
         (t . 20)
         ))
    (ivy-posframe-mode 1)
    )
  )

;; disabled to see if ill miss it
(use-package ivy-explorer
  :disabled
  :diminish
  :after ivy
  :config
  (ivy-explorer-mode 1)
  )

;; *********************************
;;
;; ** ivy-prescient
(use-package ivy-prescient
  :after ivy
  :hook
  (ivy-mode . ivy-prescient-mode)
  )

;; *********************************
;; all the icons for ivy

(use-package all-the-icons-ivy
  :after ivy
  :init
  (setq all-the-icons-ivy-file-commands
    '(ivy-switch-buffer
       counsel-find-file
       counsel-file-jump
       counsel-recentf
       counsel-projectile-find-file
       counsel-projectile-find-dir))
  :config
  (eval-when-compile
    (all-the-icons-ivy-setup))
  )

;; *********************************
;;; avy

(use-package avy
  :bind
  ("M-g j" . avy-goto-char-2)
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ;; replace native M-g c `goto-char' with `avy-goto-char'
  ([remap goto-char] . avy-goto-char)
  ;; replace native M-g g `goto-line' with `avy-goto-line'
  ;; ("M-g g" .  avy-goto-line) ;; disabled in favor of goto-line-preview
  ([remap goto-line] . avy-goto-line)
  ;; ("M-g f" .  avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  (:map isearch-mode-map
    ("C-'" . avy-search))
  (:map evil-normal-state-map
    ("SPC SPC" . avy-goto-char)
    ("SPC g c" . avy-goto-char)
    ("SPC g l" . avy-goto-line))
  (:map evil-visual-state-map
    ("SPC g l" . avy-goto-char)
    ("SPC g c" . avy-goto-line))
  :config
  (setq avy-background nil) ;; default nil ;; gray background will be added during the selection.
  (setq avy-highlight-first t) ;; When non-nil highlight the first decision char with avy-lead-face-0. Do this even when the char is terminating.
  (setq avy-all-windows nil) ;; use only the selected window
  )

;; better jumper (using to see if its good, doom-emacs uses it)

(use-package better-jumper
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (add-hook 'pre-command-hook 'better-jumper-mode)
  ;;(better-jumper-mode +1)
  (add-hook 'better-jumper-post-jump-hook #'recenter)
  )

;; *********************************-
;; Save Place

;; remember where the cursor was when opening files

;; savehist / saveplace
(use-package saveplace
  :ensure nil
  :hook
  (pre-command . save-place-mode)
  (pre-command . savehist-mode)
  :init
  ;; dont clutter the emacs folder. save somewhere else
  (setq save-place-file "~/.emacs.d/config/places")
  (setq savehist-file (concat cache-dir "savehist")
    savehist-save-minibuffer-history t
    savehist-autosave-interval nil ; save on kill only
    savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  )


;; **************************************************
;; exec path from shell

(use-package exec-path-from-shell
  :defer 1
  :config
  (exec-path-from-shell-initialize)
  )


;; *********************************
;; goto-line-preview

(use-package goto-line-preview
  ;; :config
  ;; (global-set-key [remap goto-line] 'goto-line-preview)
  )

;;****************************************************************
;;
;;; FILE / DIRECTORY NAVIGATION



;; *********************************
;;; Treemacs

(use-package treemacs
  :demand t
  :bind
  ("<f8>" . treemacs)
  :hook
  (window-setup . treemacs)
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-persist-file (expand-file-name "cache/treemacs-persist" user-emacs-directory))
  (treemacs-resize-icons 21)
  )

(use-package treemacs-evil
  :demand t
  :after treemacs evil
  :config
  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") #'treemacs-TAB-action)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-TAB-action)
  )

(use-package treemacs-projectile
  :after treemacs projectile
  )

(use-package treemacs-magit
  :after treemacs magit
  )

;; make treemacs have unique buffers for different workspaces (like for eyebrowse window setups)
(use-package treemacs-persp
  :after treemacs persp-mode
  :config (treemacs-set-scope-type 'Perspectives)
  )


;; *********************************
;;; Ranger

(use-package ranger
  :bind
  ("C-x C-j" . ranger)
  (:map evil-normal-state-map
    ("SPC f r" . ranger))
  :config
  (setq ranger-show-hidden t) ;; show hidden files
  )

;; **************************************************
;;; Dired

;; Make dired look like k

(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-dwin-target t)
  (add-hook 'dired-initial-position-hook #'dired-k)
  )

;; **************************************************
;; all the icons dired

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )



;;****************************************************************
;;
;;; TEXT MANIPULATION


;; *********************************
;;; Comment line or region

(defun comment-line-and-move-up (&optional current-line)
  "By default `comment-line' moves one line down. This function move the point back to the original location."
  (interactive)
  (if (region-active-p)
    (comment-or-uncomment-region)
    )
  ;; else
  (comment-line)
  (previous-line))

;; *********************************
;;; Duplicate line

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

(defun duplicate-line-up ()
  "Duplicate current line."
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (previous-line)
    (yank)
    (move-to-column col)))

(global-set-key [(meta shift d)] 'duplicate-line)
(global-set-key [(meta shift j)] 'duplicate-line)
(global-set-key [(control shift d)] 'duplicate-line)
(global-set-key [(control shift k)] 'duplicate-line-up)



;; *********************************
;;; expand region

;; smart selection of text

(use-package expand-region
  :bind
  ([(control shift iso-lefttab)] . er/expand-region)
  ;; ("C-=" . er/expand-region)
  )

;; *********************************
;;; subword-mode

;; change all cursor movement/edit commands to stop in-between the “camelCase” words.
;; subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.


(use-package subword
  :ensure nil
  :hook
  (js2-mode . subword-mode)
  (typescript-mode . subword-mode)
  )

;; *********************************
;;; superword-mode

;; treats text like “x_y” as one word. Useful for “snake_case”.
;; subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.

(use-package superword
  :ensure nil
  :hook
  (clojure-mode . superword-mode)
  (ruby-mode . superword-mode)
  (elixir-mode . superword-mode)
  (emacs-lisp-mode . superword-mode)
  )

;; *********************************
;;; drag stuff / move text

(use-package drag-stuff
  :bind
  ("M-k" . drag-stuff-up)
  ("M-j" . drag-stuff-down)
  ("M-h" . drag-stuff-left)
  ("M-l" . drag-stuff-right)
  :config
  (drag-stuff-mode t)
  )

;; *********************************
;; electric pair mode

;; (use-package electric-pair-mode
;;   :ensure nil
;;   :diminish
;;   :hook
;;   (prog-mode . electric-pair-mode)
;;   :config
;;   (add-hook 'prog-mode-hook
;;     (lambda ()
;;       (define-key prog-mode-map "\"" 'electric-pair)
;;       (define-key prog-mode-map "\'" 'electric-pair)
;;       (define-key prog-mode-map "(" 'electric-pair)
;;       (define-key prog-mode-map "[" 'electric-pair)
;;       (define-key prog-mode-map "{" 'electric-pair)))
;;   (add-hook 'web-mode-hook
;;     (lambda ()
;;       (define-key web-mode-map "<" 'electric-pair)))
;;   )

;; *********************************
;; smartparens

(use-package smartparens
  :diminish
  :hook
  (prog-mode . smartparens-mode)
  :config
  ;; (smartparens-global-mode +1)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (sp-pair "<" ">" :actions '(wrap))

  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; dont try to escape quotes in strings
  (setq sp-escape-quotes-after-insert nil)

  (add-hook 'minibuffer-setup-hook
    (defun init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression',
`pp-eval-expression' or `evil-ex'."
      (when (memq this-command '(eval-expression pp-eval-expression evil-ex))
        (smartparens-mode))))

  )

;; *********************************
;; open file path at point

(defun open-file-path-at-point ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2019-01-16"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                             ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                             ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
          ($path
            (replace-regexp-in-string
              "^file:///" "/"
              (replace-regexp-in-string
                ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
      (if (fboundp 'xahsite-url-to-filepath)
        (let (($x (xahsite-url-to-filepath $path)))
          (if (string-match "^http" $x )
            (browse-url $x)
            (find-file $x)))
        (progn (browse-url $path)))
      (if ; not starting “http://”
        (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
        (let (
               ($fpath (match-string 1 $path))
               ($line-num (string-to-number (match-string 2 $path))))
          (if (file-exists-p $fpath)
            (progn
              (find-file $fpath)
              (goto-char 1)
              (forward-line (1- $line-num)))
            (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
              (find-file $fpath))))
        (if (file-exists-p $path)
          (progn ; open f.ts instead of f.js
            (let (($ext (file-name-extension $path))
                   ($fnamecore (file-name-sans-extension $path)))
              (if (and (string-equal $ext "js")
                    (file-exists-p (concat $fnamecore ".ts")))
                (find-file (concat $fnamecore ".ts"))
                (find-file $path))))
          (if (file-exists-p (concat $path ".el"))
            (find-file (concat $path ".el"))
            (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
              (find-file $path ))))))))


(global-set-key (kbd "C-c C-x C-f") #'open-file-path-at-point)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC f p") #'open-file-path-at-point))

;; *********************************
;; copy file path

(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2016-07-17"
  (interactive "P")
  (let ((-fpath
          (if (equal major-mode 'dired-mode)
            (expand-file-name default-directory)
            (if (null (buffer-file-name))
              (user-error "Current buffer is not associated with a file.")
              (buffer-file-name)))))
    (kill-new
      (if (null *dir-path-only-p)
        (progn
          (message "File path copied: 「%s」" -fpath)
          -fpath
          )
        (progn
          (message "Directory path copied: 「%s」" (file-name-directory -fpath))
          (file-name-directory -fpath))))))

;;****************************************************************
;;
;; CODE NAVIGATION


;; *********************************
;;; Ctrl + mouse click jump to definition

(defun my-find-func-mouse (event)
  "Support Control + Mouse click (EVENT) to go to definition."
  (interactive "e")
  (let ((fn  (save-excursion
               (goto-char (posn-point (event-end event)))
               (function-called-at-point))))
    (unless (fboundp fn) (error "No function here"))
    (find-function-do-it fn nil 'switch-to-buffer-other-window)))

(global-set-key [C-down-mouse-1] nil)
(global-set-key [C-mouse-1] #'my-find-func-mouse)


;; *********************************
;; dumb-jump

(use-package dumb-jump
  :bind
  ;;("M-g o" . dumb-jump-go-other-window)
  ;;("M-g j" . dumb-jump-go)
  ;;("M-g b" . dumb-jump-back)
  ;;("M-g i" . dumb-jump-go-prompt)
  ;;("M-g x" . dumb-jump-go-prefer-external)
  ;;("M-g z" . dumb-jump-go-prefer-external-other-window)
  ;;("M-S-h d" . dumb-jump-hydra/body)
  (:map prog-mode-map
    ("C-c C-o" . dumb-jump-go-other-window)
    ("C-c C-j" . dumb-jump-go)
    ("C-c C-b" . dumb-jump-back)
    ("C-c C-i" . dumb-jump-go-prompt))
  (:map evil-normal-state-map
    ("SPC j g" . dumb-jump-go)
    ("SPC j b" . dumb-jump-back)
    ("SPC j o" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-selector 'ivy)
  )

;;****************************************************************
;;
;;; PACKAGE WHITESPACE

(use-package whitespace
  :ensure nil
  :defer 2
  :config
  ;; Highlight trailing whitespace
  (setq-default show-trailing-whitespace t)

  ;; Set the whitespace highlight color
  (set-face-background 'trailing-whitespace "#f44545")

  ;; Delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)


  (setq whitespace-line-column nil
    whitespace-style
    '(face indentation tabs tab-mark spaces space-mark newline newline-mark
       trailing lines-tail)
    whitespace-display-mappings
    '((tab-mark ?\t [?› ?\t])
       (newline-mark ?\n [?¬ ?\n])
       (space-mark ?\  [?·] [?.])))


  )



;;****************************************************************
;;
;; ** FONTS AND ICONS

;; set default font

;; Find the first font in the list and use it
(require 'cl) ;; this is deprecated in emacs27
(require 'cl-lib)
(require 'cl-seq) ;; for the cl-find-if function
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

;; define list of fonts to be used in the above function
;; the first one found will be used
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font-10.5"))

;; (set-face-attribute 'default nil :font (font-candidate '
;;                                          "Hack-11:weight=normal"
;;                                          "Droid Sans Mono-11:weight=normal"
;;                                          "Consolas-10:weight=normal"
;;                                          "DejaVu Sans Mono-11:weight=normal"
;;                                          "Ubuntu Mono-10:weight=normal"
;;                                          ))


;; *********************************
;; Zoom text - Increase and decrease text size
;; mouse scrolls are binded differently depending on the system
(if (eq system-type 'gnu/linux)
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
  )
(if (eq system-type 'windows-nt)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  )
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)



;;****************************************************************
;;
;; PROJECTS

;; *********************************
;;;  Projectile

(use-package projectile
  :diminish projectile-mode
  :bind
  (:map projectile-mode-map
    ("s-p" . projectile-command-map)
    ("C-c p" . projectile-command-map)
    ("M-S-O p" . counsel-projectile-switch-project)
    )
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix "Project -> ")
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (setq projectile-known-projects-file "~/.emacs.d/config/projectile-bookmarks.eld")
  ;;  NOTE: remove this
  ;; workaround for `projectile-find-file' not working because of `tmux-plugin-manager' (tpm) having a .git_modules file
  ;; when this issue is fixed remove this
  (setq projectile-git-submodule-command "")
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC p s") 'projectile-switch-project)
    (define-key evil-normal-state-map (kbd "SPC p a g") 'projectile-ag)
    (define-key evil-normal-state-map (kbd "SPC p r g") 'projectile-ripgrep))
  (projectile-mode +1)
  )


;;****************************************************************
;;
;; AUTOCOMPLETE

;; *********************************
;;; Company

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("<tab>" . company-complete-common-or-cycle)
    ([(shift iso-lefttab)] . company-select-previous))
  (:map company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next))
  :init
  (setq company-begin-commands '(self-insert-command))
  (setq company-require-match 'never)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '(company-capf))
  (setq company-frontends '(company-pseudo-tooltip-frontend
                             company-echo-metadata-frontend))
  (setq company-idle-delay 0.1) ;; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0.0) ;; remove annoying blinking
  (setq company-selection-wrap-around t) ; loop over candidates
  (setq company-tooltip-align-annotations t) ;; align annotations to the right tooltip border.
  (setq company-tooltip-margin 2) ;; width of margin columns to show around the tooltip
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; show tooltip even for single candidates
  )

;; *********************************
;; company box

;; took the entire thing from M-EMACS
(use-package company-box
  :diminish
  :hook
  (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
             (color (company-box--get-color backend))
             ((c-color a-color i-color s-color) (company-box--resolve-colors color))
             (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
             (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                 (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
             (align-string (when annotation
                             (concat " " (and company-tooltip-align-annotations
                                           (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
             (space company-box--space)
             (icon-p company-box-enable-icon)
             (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
             (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                             (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                     (company-box--apply-color icon-string i-color)
                     (company-box--apply-color candidate-string c-color)
                     align-string
                     (company-box--apply-color annotation-string a-color)))
             (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                   'company-box--color s-color)
        line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
          ((featurep sym) 'Module)
          ((facep sym) 'Color)
          ((boundp sym) 'Variable)
          ((symbolp sym) 'Text)
          (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (when (and (display-graphic-p)
          (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
      `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
         (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
         (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
         (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
         (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
         (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
         (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
         (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
         (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
         (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
         (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
         (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
         (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
         (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
         (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
         (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
         (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
         (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
         (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
         (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
         (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
         (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
         (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
         (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
         (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
         (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
         (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
      company-box-icons-alist 'company-box-icons-all-the-icons))
  )
;; *********************************
;; company quickHelp

(use-package company-quickhelp
  :disabled
  :after company
  :bind
  (:map company-active-map
    ("M-h" . company-quickhelp-manual-begin)
    )
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode)
  )

;; *********************************
;;; lsp

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (prog-mode . lsp)
  :bind
  (:map lsp-mode-map
    ;; ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ;; ([remap xref-find-references] . lsp-ui-peek-find-references)
    ("C-c u" . lsp-ui-imenu)
    ("C-c l p f r" . lsp-ui-peek-find-references)
    ("C-c l p f d" . lsp-ui-peek-find-definitions)
    ("C-c l p f i" . lsp-ui-peek-find-implementation)
    ("C-c l g d" . lsp-goto-type-definition)
    ("C-c l f d" . lsp-find-definition)
    ("C-c l g i" . lsp-goto-implementation)
    ("C-c l f i" . lsp-find-implementation)
    ("C-c l m"   . lsp-ui-imenu)
    ("C-c l s"   . lsp-ui-sideline-mode))
  :config
  ;; perfomance settings from lsp official github page
  (setq lsp-file-watch-threshold 2000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-prefer-capf t)
  (setq lsp-idle-delay 0.500)

  ;; ---
  (setq lsp-prefer-flymake :none) ;; use flycheck
  ;; no littering options
  (setq lsp-session-file "~/.emacs.d/config/lsp-session-v1")
  (setq lsp-server-install-dir (concat cache-dir "lsp"))
  ;; angular templates language server
  (setq lsp-clients-angular-language-server-command
    '("node"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules/@angular/language-server"
       "--ngProbeLocations"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules"
       "--tsProbeLocations"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules"
       "--stdio"))
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC l p f r") 'lsp-ui-peek-find-references)
    (define-key evil-normal-state-map (kbd "SPC l p f d") 'lsp-ui-peek-find-definitions)
    (define-key evil-normal-state-map (kbd "SPC l p f i") 'lsp-ui-peek-find-implementation)
    (define-key evil-normal-state-map (kbd "SPC l g d")  'lsp-goto-type-definition)
    (define-key evil-normal-state-map (kbd "SPC l d")  'lsp-find-definition)
    (define-key evil-normal-state-map (kbd "SPC l g i")  'lsp-goto-implementation)
    (define-key evil-normal-state-map (kbd "SPC l i") 'lsp-find-implementation)
    (define-key evil-normal-state-map (kbd "SPC l m") 'lsp-ui-imenu)
    (define-key evil-normal-state-map (kbd "SPC l f") 'lsp-execute-code-action)
    (define-key evil-normal-state-map (kbd "SPC l s") 'lsp-ui-sideline-mode))
  )


;; *********************************
;;; lsp ui

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  ;; :bind (:map lsp-ui-mode-map
  ;;         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ;;         ([remap xref-find-references] . lsp-ui-peek-find-references)
  ;;         ("C-c u" . lsp-ui-imenu)
  ;;         ("M-i" . lsp-ui-doc-focus-frame))
  :config
  ;; lsp-ui-doc
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-delay 1.5)
  (setq lsp-ui-doc-border (face-foreground 'default))
  ;; (setq lsp-ui-flycheck-enable t) ;; disable to leave `tslint' as checker for ts files
  ;; (setq lsp-ui-flycheck-list-position 'right)
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-code-actions-prefix "")
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  ;; Waiting for https://github.com/emacs-lsp/lsp-ui/pull/390
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  )

;; *********************************
;; company-lsp

(use-package company-lsp
  :after company lsp
  :config
  (setq company-lsp-cache-candidates 'auto)
  )


;; *********************************
;;; Yasnippets

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :bind
  ("C-<tab>" . yas-maybe-expand)
  (:map yas-minor-mode-map
    ("C-c y" . yas-expand)
    ("C-SPC" . yas-expand)
    )
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                           '("~/dotfiles/emacs.d/snippets/")))
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-reload-all) ;; tell yasnippet about updates to yas-snippet-dirs
  )

;; Colection of snippets
(use-package yasnippet-snippets)


;; *********************************
;; FlyCheck linter

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode)
  :custom-face
  (flycheck-fringe-info ((nil (:background "#007ad3" :foreground "#ffffff"))))
  (flycheck-fringe-warning ((nil (:background "#fcfa23" :foreground "#000000"))))
  (flycheck-fringe-error  ((nil (:background "#ff3300" :foreground "#000000"))))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled newline))
  (flycheck-idle-change-delay 1.5)
  (flycheck-display-errors-delay 0)
  (flycheck-indication-mode 'left-fringe)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  )


;; *********************************
;; ** flycheck posframe

(use-package flycheck-posframe
  :if (display-graphic-p)
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode)
  :custom-face
  (flycheck-posframe-info-face ((nil (:background "#007ad3" :foreground "#ffffff" :height 105))))
  (flycheck-posframe-warning-face ((nil (:background "#fcfa23" :foreground "#000000" :height 105))))
  (flycheck-posframe-error-face ((nil (:background "#000000" :foreground "#ff3300" :height 105))))
  ;; (flycheck-posframe-border-face ((nil (:foreground "#9370DB"))))
  :custom
  (flycheck-posframe-prefix "\u27a4 ") ;; default: ➤
  (flycheck-posframe-warning-prefix "\u26a0 ")
  (flycheck-posframe-info-prefix "\uf6c8 ")
  (flycheck-posframe-error-prefix "\u274c ")
  (flycheck-posframe-border-width 1)
  )


;;****************************************************************
;;
;;; HELP SYSTEM AND GUIDANCE

;; *********************************
;; helpfull

(use-package helpful
  :defer 3
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)
  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command)
  )

;; *********************************
;; paradox

(use-package paradox
  :disabled
  :config
  (paradox-enable)
  )

;; *********************************
;; which-key

(use-package which-key
  :diminish
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
    which-key-sort-uppercase-first nil
    which-key-max-display-columns nil
    which-key-side-window-slot -10)
  (setq which-key-idle-secondary-delay 0.05) ;; make it refresh quicly between displays
  (setq which-key-idle-delay 0.3)
  (setq which-key-min-display-lines 6)
  (setq which-key-add-column-padding 1)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook (lambda () (setq line-spacing 3)))
  (which-key-mode +1)
  )

;; *********************************
;; keyfreq

(use-package keyfreq
  :hook (after-init . keyfreq-mode)
  :init
  (setq keyfreq-file "~/.emacs.d/cache/.emacs.keyfreq")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

;; *********************************
;; show docstring in popup

(defun tau/elisp-function-or-variable-quickhelp (symbol)
  "Display summary of SYMBOL at point.
Adapted from `describe-function-or-variable'."
  (interactive
    (let* ((v-or-f (variable-at-point))
            (found (symbolp v-or-f))
            (v-or-f (if found v-or-f (function-called-at-point))))
      (list v-or-f)))
  (if (not (and symbol (symbolp symbol)))
    (message "You didn't specify a function or variable")
    (let* ((fdoc (when (fboundp symbol)
                   (or (documentation symbol t) "Not documented.")))
            (fdoc-short (and (stringp fdoc)
                          (substring fdoc 0 (string-match "\n" fdoc))))
            (vdoc (when  (boundp symbol)
                    (or (documentation-property symbol 'variable-documentation t)
                      "Not documented as a variable.")))
            (vdoc-short (and (stringp vdoc)
                          (substring vdoc 0 (string-match "\n" vdoc)))))
      (and (require 'popup nil 'no-error)
        (popup-tip
          (or
            (and fdoc-short vdoc-short
              (concat fdoc-short "\n\n"
                (make-string 30 ?-) "\n" (symbol-name symbol)
                " is also a " "variable." "\n\n"
                vdoc-short))
            fdoc-short
            vdoc-short)
          :margin t)))))


;;****************************************************************
;;
;; PACKAGES

;; Native Packages


;; **************************************
;; ansi-color

(use-package ansi-color
  :disabled
  :ensure nil
  :preface
  (defun tau-colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  ;; Handle ansi codes in compilation buffer
  (compilation-filter-hook . tau-colorize-compilation-buffer)
  :init
  (setq ansi-color-for-comint-mode t)
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
    compilation-ask-about-save nil  ; save all buffers on `compile'
    compilation-scroll-output 'first-error)
  )


;; *********************************
;; hippie expand

(use-package hippie-exp
  :ensure nil
  :bind
  ("<tab>" . hippie-expand)
  ("<C-return>" . hippie-expand)
  (:map evil-insert-state-map
    ("<tab>" . hippie-expand)
    )
  :init
  (setq-default hippie-expand-try-functions-list
    '(
       yas-hippie-try-expand
       company-indent-or-complete-common
       emmet-expand-line
       emmet-expand-yas
       indent-according-to-mode
       ))
  :config
  (:map evil-insert-state-map
    ("<tab>" . company-indent-or-complete-common))
  )


;; *********************************
;; auto revert mode

(use-package autorevert
  :ensure nil
  :defer 2
  :preface
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))

  (defun revert-buffer-maybe (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))
  :hook
  ;; revert buffers when their files/state have changed
  (focus-in . revert-buffer-maybe)
  ;;(after-save . revert-buffer-maybe)
  (switch-buffer . revert-buffer-maybe)
  (switch-window . revert-buffer-maybe)
  :config
  (setq auto-revert-verbose t) ; let us know when it happens
  (setq auto-revert-use-notify nil)
  (setq auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (setq revert-without-query (list "*.*"))
  ;; auto revert timer
  ;; (setq auto-revert-interval 1) ;; rever every 1 second
  ;; (auto-revert-set-timer) ;; this function needs to be called after setting the `revert-interval' variable above
  (setq auto-revert-check-vc-info t)
  )

;; *********************************
;; aggressive indent

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (prog-mode . aggressive-indent-mode)
  (css-mode . aggressive-indent-mode)
  :config
  (setq aggressive-indent-comments-too t)
  )


;; *********************************
;; conf mode

(use-package conf-mode
  :ensure nil
  :mode
  (;; systemd
    ("\\.service\\'"     . conf-unix-mode)
    ("\\.timer\\'"      . conf-unix-mode)
    ("\\.target\\'"     . conf-unix-mode)
    ("\\.mount\\'"      . conf-unix-mode)
    ("\\.automount\\'"  . conf-unix-mode)
    ("\\.slice\\'"      . conf-unix-mode)
    ("\\.socket\\'"     . conf-unix-mode)
    ("\\.path\\'"       . conf-unix-mode)

    ;; general
    ("conf\\(ig\\)?$"   . conf-mode)
    ("rc$"              . conf-mode))
  )



;; *********************************
;; vi tilde fringe

(use-package vi-tilde-fringe
  :disabled
  :diminish
  :config
  (prog-mode . vi-tilde-fringe-mode)
  (text-mode . vi-tilde-fringe-mode)
  )


;;****************************************************************
;;
;;; Terminal and Shell Related Settings

;; *********************************
;; shell script mode

(use-package sh-script
  :ensure nil
  :hook
  (sh-mode . aggressive-indent-mode)
  (sh-mode . rainbow-mode)
  )

;; *********************************
;; shell-pop

(use-package shell-pop
  :init
  (setq shell-pop-full-span t)
  (setq shell-pop-default-directory "~/code")
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (setq shell-pop-universal-key "C-c s")
  (setq shell-pop-window-size 30)
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  :bind
  ("C-c s" . shell-pop)
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC !") 'shell-pop))
  )

;; Execute selected line or marked region as command in system shell

(defun shell-command-on-region-or-line ()
  "Run selected text in a terminal or use the current line."
  (interactive)
  (shell-command
    (concat
      ;; pick one!
      "st -e "
      ;; "gnome-terminal -e "
      ;; "roxterm --tab -e "
      ;; "terminator -x "
      (if (use-region-p)
        ;; current selection
        (buffer-substring (region-beginning) (region-end))
        ;; current line
        (thing-at-point 'line t)))))

(global-set-key (kbd "<C-M-S-return>") 'shell-command-on-region-or-line)



;;****************************************************************
;;
;; UI RELATED PACKAGES

;; *********************************
;; Centaur tabs

(use-package centaur-tabs
  :if (< emacs-major-version 26)
  ;;:if (version<= "27.1" emacs-version)
  :hook
  (window-setup . centaur-tabs-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ;; ("C-x p" . centaur-tabs-counsel-switch-group)
  :config
  (setq centaur-tabs-set-bar 'under) ;; display an underline over the selected tab:
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-modified-marker t) ;; display a marker indicating that a buffer has been modified (atom-style)
  (setq centaur-tabs-modified-marker " ● ")
  (setq centaur-tabs-close-button " × ")
  (setq centaur-tabs-cycle-scope 'tabs) ;; dont change tabs groups, cicle through
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-icons t) ;; use icons from all the icons
  (setq centaur-tabs-show-navigation-buttons t) ;; display cool navigations buttons
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  ;; (when (member "Arial" (font-family-list))
  ;;   (centaur-tabs-change-fonts "Arial" 130))

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
    (define-key evil-normal-state-map (kbd "SPC !") 'centaur-tabs-backward))
  )



;; *********************************
;; Solaire mode

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  ;;(solaire-mode-swap-bg)
  )


;; **************************************
;; all-the-icons

(use-package all-the-icons
  :if window-system
  :commands
  (all-the-icons-octicon
    all-the-icons-faicon
    all-the-icons-fileicon
    all-the-icons-wicon
    all-the-icons-material
    all-the-icons-alltheicon)
  )

;; **************************************
;; goto address

(use-package goto-addr
  :ensure nil
  :hook (text-mode . goto-address-mode)
  :hook (prog-mode . goto-address-prog-mode)
  :config
  (define-key goto-address-highlight-keymap (kbd "RET") #'goto-address-at-point)
  )



;;****************************************************************
;;
;; WEB DEVELOPMENT MAJOR MODES

;; *********************************
;;
;; Typescript Mode

(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.tsx\\'" . typescript-mode)
  )

;; *********************************
;;
;; js2-mode

(use-package js2-mode
  :mode
  ("\\.js$" . js2-mode)
  :init
  ;; have 2 space indentation by default
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js-chain-indent t)
  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)
  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  )

;; *********************************
;;
;;; Angular

;; Angular Open Counterpart (taken from ng2-mode)
(defun angular--counterpart-name (file)
  "Return the file name of FILE's counterpart, or FILE if there is no counterpart."
  (when (not (angular--is-component file)) file)
  (let ((ext (file-name-extension file))
         (base (file-name-sans-extension file)))
    (if (equal ext "ts")
      (concat base ".html")
      (concat base ".ts"))))

(defun angular--is-component (file)
  "Return whether FILE is a component file."
  (equal (file-name-extension (file-name-sans-extension file)) "component"))

(defun angular-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (angular--counterpart-name (buffer-file-name))))

;; Open templates under point

(defun angular-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (angular--counterpart-name (buffer-file-name))))
(global-set-key (kbd "C-x a c") #'angular-open-counterpart)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC a c") #'angular-open-counterpart))


;; *********************************
;;
;; Web-Mode

(use-package web-mode
  :mode
  ("\\.component.html\\'" . web-mode)
  ("\\.phtml\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.[t]?html?\\'" . web-mode)
  :custom
  ;; (web-mode-markup-indent-offset 2)
  ;; (web-mode-css-indent-offset 2)
  ;; (web-mode-code-indent-offset 2)
  ;; (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-block-face t)
  (web-mode-enable-part-face t)
  :config
  ;; Template
  (setq web-mode-engines-alist
    '(("php"    . "\\.phtml\\'")
       ("blade"  . "\\.blade\\.")))
  )

;; *********************************
;;
;; CSS

(use-package css-mode
  :ensure nil
  :mode
  ("\\.css\\'" . css-mode)
  ("\\.rasi\\'" . css-mode) ;; support for rofi new config format
  :config
  (setq css-indent-offset 2)
  )


;; *********************************
;;
;; SCSS

(use-package scss-mode
  ;; this mode doenst load using :mode from use-package, dunno why
  :mode ("\\.scss\\'")
  :init
  (setq scss-compile-at-save 'nil)
  :config
  ;; (autoload 'scss-mode "scss-mode")
  ;; (setq scss-compile-at-save 'nil)
  ;; (add-to-list 'auto-mode-alist '("\\.scss$\\'" . scss-mode))
  )


;;****************************************************************
;;
;; WEB DEVELOPMENT TOOLS


;; *********************************
;; editorconfig

(use-package editorconfig
  :diminish
  :hook
  (typescript-mode . editorconfig-mode)
  (js2-mode . editorconfig-mode)
  (web-mode . editorconfig-mode)
  (css-mode . editorconfig-mode)
  (scss-mode . editorconfig-mode)
  (go-mode . editorconfig-mode)
  )


;; *********************************
;;
;; PrettierJS

(use-package prettier-js
  :hook
  (typescript-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (css-mode . prettier-js-mode)
  :custom
  (prettier-js-show-errors 'buffer) ;; options: 'buffer, 'echo or nil
  :config
  )

;; *********************************
;; Emmet

(use-package emmet-mode
  :commands emmet-mode
  :hook
  (typescript-mode . emmet-mode)
  (web-mode . emmet-mode)
  (css-mode . emmet-mode)
  :config
  ;; (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? nil) ;; use emmet with JSX markup
  )

;;****************************************************************
;;
;; BACKEND TOOLS

;; *********************************
;; Docker

(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'" . dockerfile-mode)
  )

(use-package docker-compose-mode)

;;****************************************************************
;;
;; LANGUAGES SETUP

;; *********************************
;; go (golang) mode


(use-package go-mode
  :mode
  ("\\.go$" . go-mode)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  )


;; *********************************
;; lisp mode

(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :hook
  (elisp-mode . eldoc-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  (bind-key "C-c c" 'compile emacs-lisp-mode-map)
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  (setq-default lisp-indent-offset 2)
  )


;; *********************************
;; lisp mode

(use-package haskell-mode
  :mode
  ("\\.hs\\'")
  )


;; *********************************
;; YAML

(use-package yaml-mode
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  )

;; *********************************
;; vimrc

(use-package vimrc-mode
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  ("\\vim\\(rc\\)?\\'" . vimrc-mode)
  )


;;****************************************************************
;;
;;; VERSION CONTROL

;; *********************************
;; magit

(use-package magit
  :defer 2
  :bind
  ("M-g s" . magit-status)
  ("M-g f" . magit-find-file)
  ("M-g l" . magit-log)
  ("M-g b" . magit-blame)
  ("C-x g" . magit-status)
  (:map magit-mode-map
    ("<tab>" . magit-section-toggle))
  (:map evil-normal-state-map
    ("SPC m b" . magit-blame)
    ("SPC m s" . magit-status))
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC m b") 'magit-blame)
    (define-key evil-normal-state-map (kbd "SPC m s") 'magit-status))

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  ;; integrate with vc-msg
  (eval-after-load 'vc-msg-git
    '(progn
       ;; show code of commit
       (setq vc-msg-git-show-commit-function 'magit-show-commit)
       ;; open file of certain revision
       (push '("m"
                "[m]agit-find-file"
                (lambda ()
                  (let* ((info vc-msg-previous-commit-info)
                          (git-dir (locate-dominating-file default-directory ".git")))
                    (magit-find-file (plist-get info :id )
                      (concat git-dir (plist-get info :filename))))))
         vc-msg-git-extra)))
  )

;; *********************************
;; evil-magit

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  :config
  (evil-magit-init)
  (evil-define-key evil-magit-state magit-mode-map "<tab>" 'magit-section-toggle)
  (evil-define-key evil-magit-state magit-mode-map "l" 'magit-log-popup)
  (evil-define-key evil-magit-state magit-mode-map "j" 'evil-next-visual-line)
  (evil-define-key evil-magit-state magit-mode-map "k" 'evil-previous-visual-line)
  ;; (evil-define-key evil-magit-state magit-diff-map "k" 'evil-previous-visual-line)
  (evil-define-key evil-magit-state magit-staged-section-map "K" 'magit-discard)
  (evil-define-key evil-magit-state magit-unstaged-section-map "K" 'magit-discard)
  (evil-define-key evil-magit-state magit-branch-section-map "K" 'magit-branch-delete)
  (evil-define-key evil-magit-state magit-remote-section-map "K" 'magit-remote-remove)
  (evil-define-key evil-magit-state magit-stash-section-map "K" 'magit-stash-drop)
  (evil-define-key evil-magit-state magit-stashes-section-map "K" 'magit-stash-clear)
  )


;; *********************************
;; diffview
;; View diffs side by side

(use-package diffview)

;; *********************************
;; vc-msg

(use-package vc-msg
  :bind
  ("C-c g p" . vc-msg-show)
  (:map evil-normal-state-map
    ("SPC g m" . git-timemachine-toggle))
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t)
  :config
  (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)
  (add-hook 'activate-mark-hook '(lambda () (vc-msg-show)))
  (add-hook 'mouse-position-function '(lambda () (vc-msg-show)))
  )

;; *********************************
;;
;; git timemachine

(use-package git-timemachine
  :bind
  ("C-c g t" . git-timemachine-toggle)
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC g t") 'git-timemachine-toggle))
  )

;; *********************************
;;
;; diff-hl (highlights uncommited diffs in the fringe)

(use-package diff-hl
  :defer 1
  :custom-face
  ;; Better looking colours for diff indicators
  ;;(diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  (diff-hl-change ((t (:background "#007ad3" :foreground "#ffffff"))))
  (diff-hl-insert ((t (:background "#00ff00" :foreground "#000000"))))
  (diff-hl-delete ((t (:background "#ff3300" :foreground "#ffffff"))))
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;;(diff-hl-flydiff-mode) ;; highlighting changes on the fly
  ;; (diff-hl-margin-mode) ;; use the margin instead of the fringe.
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
      '((insert . " ") (delete . " ") (change . " ")
         (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
        '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  )

;; Mode for .gitignore files.
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)


;;****************************************************************
;;
;;; UI -> Modeline

;; (use-package doom-modeline
;;   :hook
;;   (after-init . doom-modeline-mode)
;;   :init
;;   (doom-modeline-mode 1)
;;   )

;; **************************************
;; hide-mode-line-mode

(use-package hide-mode-line
  :hook
  (Man-mode . hide-mode-line-mode)
  (completion-list-mode . hide-mode-line-mode)
  (treemacs-mode . hide-mode-line-mode)
  )


;; **************************************
;; my personal modeline

;; (use-package tau-modeline
;; :ensure nil
;; )

;; ##################################################
;;; Emacs startup profiler

(use-package esup
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  ;; :commands is Only needed for functions without an autoload comment (;;;###autoload).
  :commands (esup))


;; (use-package gcmh
;;   :init
;;   ;; (setq gcmh-verbose             t
;;   ;;       gcmh-lows-cons-threshold #x800000
;;   ;;       gcmh-high-cons-threshold most-positive-fixnum
;;   ;;       gcmh-idle-delay          3600)
;;   :config
;;   (gcmh-mode 1)
;;   )
