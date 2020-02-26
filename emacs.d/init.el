;;; Package --- Summary  -*- lexical-binding: t; -*-


;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

(when (version< emacs-version "26.1")
  (error "Detected Emacs %s. This emacs config only supports Emacs 26.1 and higher"
    emacs-version))

(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))


;;
;;; Global variables

(defvar tau-init-p nil
  "Non-nil if Doom has been initialized.")

(defvar tau-init-time nil
  "The time it took, in seconds, for Doom Emacs to initialize.")

(defvar tau-is-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Doom will log more.
Use `doom/toggle-debug-mode' to toggle it. The --debug-init flag and setting the
DEBUG envvar will enable this at startup.")

(defvar tau-is-interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Garbage Collection GC Settings

;; source: https://raw.githubusercontent.com/gilbertw1/emacs-literate-starter/master/emacs.org

;; defer GC on startup

(setq gc-cons-threshold most-positive-fixnum ;; 2^61 bytes
  gc-cons-percentage 0.6
garbage-collection-messages t)

;; reset gc after emacs loads

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
      gc-cons-percentage 0.1)))


;;
;; It may also be wise to raise gc-cons-threshold while the minibuffer is active, so the GC doesn’t slow down expensive commands (or completion frameworks, like helm and ivy). Here is how Doom does it:

;; store emacs original gc value in a variable
(defvar tau-gc-cons-threshold gc-cons-threshold)

(defun tau-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun tau-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
    1 nil (lambda () (setq gc-cons-threshold tau-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'tau-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'tau-restore-garbage-collection-h)

;; Unset file-name-handler-alist temporarily.
;; Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but during startup, it won’t need any of them.

(defvar tau--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


;; restore it later:
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist tau--file-name-handler-alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BOOTSTRAP PACKAGE MANAGEMENT


;; New emacs 27 'package-quickstart' feature
;; package.el precomputes a big autoloads file so that activation of packages can be done much faster. It also causes variables like package-user-dir and package-load-list to be consulted when 'package-quickstart-refresh' is run rather than at startup so you don't need to set them in your early init file.

(when EMACS27+
  (setq package-quickstart t))


;; We're going to set the =load-path= ourselves and avoid calling =(package-initilize)= (forperformance reasons) so we need to set =package--init-file-ensured= to true to tell =package.el= to not automatically call it on our behalf. Additionally we're setting =package-enable-at-startup= to nil so that packages will not automatically be loaded for us since =use-package= will be handling that.

;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(eval-and-compile
  (setq load-prefer-newer t
    package-user-dir (concat user-emacs-directory "elpa/")
    package-enable-at-startup nil
    package--init-file-ensured t)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))
;; -------------------------------------


;; Manually Set Load Path
;;
;; We're going to set the load path ourselves so that we don't have to call =package-initialize= at
;; runtime and incur a large performance hit. This load-path will actually be faster than the one
;; created by =package-initialize= because it appends the elpa packages to the end of the load path.
;; Otherwise any time a builtin package was required it would have to search all of third party paths
;; first.

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

;; -------------------------------------

;; Initialize Package Management
;;
;; Require package.el, add melpa and org archives and install and load use-package if not already installed

;; Using `eval-when-compile' to perform all of the package initialization during compilation so that when byte compiled, all of this time consuming
;; code is skipped.
;; This can be done because the result of byte compiling =use-package= statements results
;; in the macro being fully expanded at which point =use-package= isn't actually required any longer.

;; Since the code is automatically compiled during runtime, if the configuration hasn't already been
;; previously compiled manually then all of the package initialization will still take place at startup.

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
  (setq use-package-always-defer t
    use-package-verbose t
    use-package-always-ensure t
    use-package-enable-imenu-support t
    use-package-minimum-reported-time 0
    use-package-verbose t
    use-package-compute-statistics t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(when tau-is-interactive-mode
  (add-hook 'pre-command-hook (lambda () (gcmh-mode +1)))
  (with-eval-after-load 'gcmh
    (setq gcmh-idle-delay 10
      gcmh-verbose nil ;; this was using doom-debug-mode, which appeared to be only nil
      gcmh-high-cons-threshold 16777216) ; 16mb
    (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later.
(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (defun tau-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; EMACS CORE EDITOR SETTINGS

(setq ring-bell-function 'ignore) ;; Disable the annoying Emacs bell ring (beep)


;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

(setq kill-do-not-save-duplicates t) ;; Eliminate duplicates in the kill ring.

(setq next-line-add-newlines t) ;; C-n insert newlines if the point is at the end of the buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(load-theme my-theme t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; SCROLLING SETTINGS

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
  auto-window-vscroll nil
  ;; mouse
  mouse-wheel-scroll-amount '(5 ((shift) . 2))
  mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(add-hook 'eshell-mode-hook (lambda () (hscroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (hscroll-margin 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Eyebrowse

(use-package eyebrowse
  :config
  (eval-when-compile
    (eyebrowse-mode t)
    (setq eyebrowse-new-workspace t)
    (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4))
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
  (eval-when-compile
    (show-paren-mode +1))
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
;; package image

(setq image-animate-loop t)

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
  :disabled
  :ensure nil
  :config
  (tab-bar-mode +1) ;; per-frame
  ;; (tab-line-mode +1) ;; per window
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; -------------------------------------

;;
;; OTHER SETTINGS

(use-package diminish)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Emacs settings


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Folders and Files

;;; Define emacs directory
(defconst emacs-dir (eval-when-compile (file-truename user-emacs-directory)))

;; Define a config folder inside emacs-dir
(defconst config-dir (eval-when-compile (concat emacs-dir "config/")))

;; Define a config folder inside emacs-dir
(defconst cache-dir (eval-when-compile (concat emacs-dir "cache/")))

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
  :defer 4
  :ensure nil
  :config
  (setq abbrev-file-name (concat config-dir "abbrev.el"))
  )



(use-package recentf
  :ensure nil
  ;; Loads after 1 second of idle time.
  :defer 1
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Evil

(use-package evil
  :demand t
  :config
  (eval-when-compile
    (evil-mode 1)
    ;; change cursor color according to mode
    (setq evil-emacs-state-cursor '("#ff0000" box))
    (setq evil-motion-state-cursor '("#FFFFFF" box))
    (setq evil-normal-state-cursor '("#00ff00" box))
    (setq evil-visual-state-cursor '("#abcdef" box))
    (setq evil-insert-state-cursor '("#e2f00f" bar))
    (setq evil-replace-state-cursor '("#ff0000" hbar))
    (setq evil-operator-state-cursor '("#ff0000" hollow))
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

    ;; ------------------------------------------
    ;;
  ;;; RECOVER SOME USEFUL EMACS NATIVE COMMANDS
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-normal-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    ;; native find definition
    (global-set-key (kbd "M-.") 'xref-find-definitions)
    ;; somehow global-set-key doenst work for normal mode for this binding, so i manually set it
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

    (global-set-key (kbd "C-S-H") 'evil-window-left)
    (global-set-key (kbd "C-S-L") 'evil-window-right)
    (global-set-key (kbd "C-S-K") 'evil-window-up)
    (global-set-key (kbd "C-S-J") 'evil-window-down)
    )
  )


;; *********************************
;; undo tree

(use-package undo-tree
  ;; Branching & persistent undo
  :after-call doom-switch-buffer-hook after-find-file
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

(if (EMACS27+)
  (global-so-long-mode 1)
  (use-package so-long
    :config
    (global-so-long-mode 1)
    )
  )

;; *********************************
;; evil-commentary

(use-package evil-commentary
  :after evil
  :diminish
  :hook
  (evil-mode . evil-commentary-mode)
  ;;:config
  ;; (evil-commentary-mode)
  )

;; *********************************
;; evil-surround
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

;; *********************************
;; evil-surround

(use-package evil-surround
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  ;; display an arrow on the selected item in the list
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  ;; SPC keybindings
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC b") 'counsel-switch-buffer)
    (define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)
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
:demand t
  :after ivy
  :diminish counsel-mode
  :hook
  (ivy-mode . counsel-mode)
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
  :init
  (setq amx-save-file "~/.emacs.d/config/amx-items")
  :config
  (eval-when-compile
    (amx-mode))
  )

;; *********************************
;; ivy-rich

(use-package ivy-rich
  :after ivy
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
  (setq ivy-rich--original-display-transformers-list nil)  ;; needs to be set otherwise (ivy-rich-set-display-transformer) does not get called

  ;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
  (setq ivy-rich-path-style 'abbrev)

  (setq ivy-rich--display-transformers-list
    '(ivy-switch-buffer
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
       counsel-find-file
       (:columns
         ((ivy-read-file-transformer)
           (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
       counsel-M-x
       (:columns
         ((counsel-M-x-transformer (:width 35))
           (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
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
  (ivy-rich-mode 1)
  )

;; *********************************
;; ivy-posframe

;; Requires: Emacs >= 26

(use-package ivy-posframe
  :after ivy
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe ((t (:background "#202020"))))
  (ivy-posframe-border ((t (:background "#9370DB"))))
  (ivy-posframe-cursor ((t (:background "#00ff00"))))
  :init
  (setq ivy-posframe-parameters '((internal-border-width . 1)))
  (setq ivy-posframe-width 130)
  (setq ivy-posframe-parameters '((alpha . 85)))
  ;; define the position of the posframe per function
  (setq ivy-posframe-display-functions-alist
    '(
       (counsel-M-x     . nil)
       (swiper          . nil)
       (complete-symbol . ivy-posframe-display-at-point)
       (t               . ivy-posframe-display-at-frame-center)
       ))
  ;; custom define height of post frame per function
  (setq ivy-posframe-height-alist '(
                                     (find-file . 15)
                                     (counsel-ag . 15)
                                     (counsel-projectile-ag . 30)
                                     (t      . 20)
                                     ))
  :config
  (eval-when-compile
    (ivy-posframe-mode))
  )

(use-package ivy-explorer
  :diminish
  :after ivy
  :config
  (eval-when-compile
    (ivy-explorer-mode 1))
  )

;; *********************************
;;
;; ** ivy-prescient
(use-package ivy-prescient
  :after ivy
  :config
  (eval-when-compile
    (ivy-prescient-mode))
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
  (add-hook pre-command-hook 'better-jumper-mode)
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
  (setq-default save-place t)
  ;; dont clutter the emacs folder. save somewhere else
  (setq save-place-file "~/.emacs.d/config/places")
  (setq savehist-file (concat cache-dir "savehist")
    savehist-save-minibuffer-history t
    savehist-autosave-interval nil ; save on kill only
    savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  )


;; **************************************************
;;
;; (use-package exec-path-from-shell
;;   :demand t
;;   :config
;;   (exec-path-from-shell-initialize)
;;   )

;; Attempt to get env vars without exec-path-from-shell, since its very slow

;;
;; Helpers

(defvar tau-env-ignored-vars
  '("^DBUS_SESSION_BUS_ADDRESS$"
     "^GPG_AGENT_INFO$"
     "^GPG_TTY$"
     "^HOME$"
     "^PS1$"
     "^PWD$"
     "^R?PROMPT$"
     "^SSH_AGENT_PID$"
     "^SSH_AUTH_SOCK$"
     "^TERM$"
     ;; Doom envvars
     "^DEBUG$"
     "^INSECURE$"
     "^YES$"
     "^__")
  "Environment variables to not save in `doom-env-file'.
Each string is a regexp, matched against variable names to omit from
`doom-env-file'.")

(defun tau-cli-reload-env-file (&optional force-p env-file)
  "Generates `doom-env-file', if it doesn't exist (or if FORCE-P).
This scrapes the variables from your shell environment by running
`doom-env-executable' through `shell-file-name' with `doom-env-switches'. By
default, on Linux, this is '$SHELL -ic /usr/bin/env'. Variables in
`doom-env-ignored-vars' are removed."
  (let ((env-file (if env-file
                    (expand-file-name env-file)
                    doom-env-file)))
    (when (or force-p (not (file-exists-p env-file)))
      (with-temp-file env-file
        (print! (start "%s envvars file at %S")
          (if (file-exists-p env-file)
            "Regenerating"
            "Generating")
          (path env-file))
        (let ((process-environment doom--initial-process-environment))
          (print! (info "Scraping shell environment"))
          (print-group!
            (when tau-is-interactive-mode
              (user-error "'doom env' must be run on the command line, not an interactive session"))
            (goto-char (point-min))
            (insert
              (concat
                "# -*- mode: sh -*-\n"
                (format "# Generated from a %s shell environent\n" shell-file-name)
                "# ---------------------------------------------------------------------------\n"
                "# This file was auto-generated by `doom env'. It contains a list of environment\n"
                "# variables scraped from your default shell (excluding variables blacklisted\n"
                "# in doom-env-ignored-vars).\n"
                "#\n"
                (if (file-equal-p env-file tau-env-file)
                  (concat "# It is NOT safe to edit this file. Changes will be overwritten next time you\n"
                    "# run 'doom sync'. To create a safe-to-edit envvar file use:\n#\n"
                    "#   doom env -o ~/.doom.d/myenv\n#\n"
                    "# And load it with (doom-load-envvars-file \"~/.doom.d/myenv\").\n")
                  (concat "# This file is safe to edit by hand, but needs to be loaded manually with:\n#\n"
                    "#   (doom-load-envvars-file \"path/to/this/file\")\n#\n"
                    "# Use 'doom env -o path/to/this/file' to regenerate it."))
                "# ---------------------------------------------------------------------------\n\n"))
            ;; We assume that this noninteractive session was spawned from the
            ;; user's interactive shell, therefore we just dump
            ;; `process-environment' to a file.
            (dolist (env process-environment)
              (if (cl-find-if (doom-rpartial #'string-match-p (car (split-string env "=")))
                    tau-env-ignored-vars)
                (print! (info "Ignoring %s") env)
                (insert env "\n")))
            (print! (success "Successfully generated %S")
              (path env-file))
            t))))))

;; *********************************
;; goto-line-preview

(use-package goto-line-preview
  ;; :config
  ;; (global-set-key [remap goto-line] 'goto-line-preview)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; FILE / DIRECTORY NAVIGATION



;; *********************************
;;; Treemacs

(use-package treemacs
  :bind
  ("<f8>" . treemacs)
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-persist-file (expand-file-name "cache/treemacs-persist" user-emacs-directory))
  (treemacs-resize-icons 21)
  )

(use-package treemacs-evil
  :after treemacs evil
  )

(use-package treemacs-projectile
  :after treemacs projectile
  )

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   )

;; (use-package treemacs-persp
;;   :after treemacs persp-mode
;;   :config (treemacs-set-scope-type 'Perspectives))


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TEXT MANIPULATION


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

(global-set-key [(meta shift d)] 'duplicate-line)
(global-set-key [(control shift d)] 'duplicate-line)



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

(use-package electric-pair-mode
  :ensure nil
  :diminish
  :hook
  (prog-mode . electric-pair-mode)
  :config
  (add-hook 'prog-mode-hook
    (lambda ()
      (define-key prog-mode-map "\"" 'electric-pair)
      (define-key prog-mode-map "\'" 'electric-pair)
      (define-key prog-mode-map "(" 'electric-pair)
      (define-key prog-mode-map "[" 'electric-pair)
      (define-key prog-mode-map "{" 'electric-pair)))
  (add-hook 'web-mode-hook
    (lambda ()
      (define-key web-mode-map "<" 'electric-pair)))
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
(define-key evil-normal-state-map (kbd "SPC f p") #'open-file-path-at-point)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :config
  (eval-when-compile
    (require 'helm-source nil t))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; PACKAGE WHITESPACE

(use-package whitespace
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(add-to-list 'default-frame-alist '(font . "Hack-10.5"))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (:map evil-normal-state-map
    ("SPC p s" . projectile-switch-project)
    ("SPC p a g" . projectile-ag)
    ("SPC p r g" . projectile-ripgrep))
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
  (eval-when-compile
    (projectile-mode +1))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUTOCOMPLETE

;; *********************************
;;; Company

(use-package company
  :demand t
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
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1) ;; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0) ;; remove annoying blinking
  (setq company-selection-wrap-around t) ; loop over candidates
  (setq company-tooltip-align-annotations t) ;; align annotations to the right tooltip border.
  (setq company-tooltip-margin 2) ;; width of margin columns to show around the tooltip
  :config
  (global-company-mode)
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; show tooltip even for single candidates
  (setq company-frontends '(company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  )

;; *********************************
;; company box

(use-package company-box
  :after company
  :hook
  ((global-company-mode company-mode) . company-box-mode)
  )


;; *********************************
;; company quickHelp

(use-package company-quickhelp
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
  (typescript-mode . lsp)
  (web-mode . lsp)
  (scss-mode . lsp)
  (css-mode . lsp)
  (js2-mode . lsp)
  (go-mode . lsp)
  (yaml-mode . lsp)
  (sh-mode . lsp)
  (conf-mode . lsp)
  :bind
  (:map evil-normal-state-map
    ("SPC l p f r" . lsp-ui-peek-find-references)
    ("SPC l p f d" . lsp-ui-peek-find-definitions)
    ("SPC l p f i" . lsp-ui-peek-find-implementation)
    ("SPC l g d" . lsp-goto-type-definition)
    ("SPC l d" . lsp-find-definition)
    ("SPC l g i" . lsp-goto-implementation)
    ("SPC l i" . lsp-find-implementation)
    ("SPC l m"   . lsp-ui-imenu)
    ("SPC l f"   . lsp-execute-code-action)
    ("SPC l s"   . lsp-ui-sideline-mode))
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
  ;; general
  (setq lsp-auto-configure t) ;; auto-configure `lsp-ui' and `company-lsp'
  (setq lsp-enable-indentation nil) ;; NOTE: indentation with lsp is super slow on emacs 26. try again no future versions
  (setq lsp-prefer-flymake :none) ;; t: flymake | nil: lsp-ui | :none -> none of them (flycheck)
  (setq lsp-session-file "~/.emacs.d/config/lsp-session-v1") ;; t: flymake | nil: lsp-ui | :none -> none of them (flycheck)
  ;; angular language server
  ;; this serves for editing templates only (both inline and external)
  (setq lsp-clients-angular-language-server-command
    '("node"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules/@angular/language-server"
       "--ngProbeLocations"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules"
       "--tsProbeLocations"
       "/home/tau/.nvm/versions/node/v10.9.0/lib/node_modules"
       "--stdio"))
  ;; disable angular-ls for specific modes
  ;; (add-to-list 'lsp-disabled-clients '(web-mode . angular-ls))
  (setq lsp-server-install-dir (concat cache-dir "lsp"))
  )


;; *********************************
;;; lsp ui

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t) ;; disable to leave `tslint' as checker for ts files
  (setq lsp-ui-flycheck-list-position 'right)

  (setq lsp-ui-doc-enable nil)
  (add-hook 'emacs-lisp-hook
    (lambda ()
      (setq lsp-ui-doc-enable t)))

  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-diagnostics t) ;; show diagnostics (flyckeck) messages in sideline
  (setq lsp-ui-sideline-code-actions-prefix "")

  ;; (setq lsp-ui-imenu-enable t)
  ;; ;; (setq lsp-ui-imenu-kind-position 'top)

  ;; (setq lsp-ui-peek-enable t)
  ;; ;; (setq lsp-ui-peek-peek-height 20)
  ;; ;; (setq lsp-ui-peek-list-width 40)

  ;; ;; lsp-ui doc frame appearance
  ;; (add-hook 'lsp-ui-doc-frame-hook
  ;;   (lambda (frame _w)
  ;;     (set-face-attribute 'default frame :font "Hack 11")))

  ;; ;; Use lsp-ui-doc-webkit only in GUI
  ;; (if (display-graphic-p)
  ;;   (setq lsp-ui-doc-use-webkit t))
  )

;; *********************************
;; company-lsp

;; company-lsp is auto inserted into company backends

(use-package company-lsp
  :after company lsp)


;; *********************************
;;; Yasnippets

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :bind
  ;; ("<tab>" . yas-maybe-expand)
  ("C-<tab>" . yas-maybe-expand)
  (:map yas-minor-mode-map
    ;; yas-maybe-expand only expands if there are candidates.
    ;; if not, acts like binding is unbound and run whatever command is bound to that key normally
    ;; ("<tab>" . yas-maybe-expand)
    ;; Bind `C-c y' to `yas-expand' ONLY.
    ("C-c y" . yas-expand)
    ("C-SPC" . yas-expand)
    )
  :config
  ;; add angular snippets folder (code taken from oficial docs page)
  (setq yas-snippet-dirs (append yas-snippet-dirs
                           '("~/dotfiles/emacs.d/snippets/")))
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (yas-reload-all) ;; tell yasnippet about updates to yas-snippet-dirs
  ;; disabled global mode in favor or hooks in prog and text modes only
  ;; (yas-global-mode 1)
  )

;;
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
  (flycheck-display-errors-delay 1)
  (flycheck-indication-mode 'left-fringe)
  :config
  ;; (global-flycheck-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  )


;; *********************************
;; ** flycheck posframe

(use-package flycheck-posframe
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (add-hook 'which-key-init-buffer-hook (lambda () (line-spacing 3)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGES

;; Native Packages


;; **************************************
;; ansi-color

(use-package ansi-color
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
  (eval-when-compile
    (setq compilation-always-kill t       ; kill compilation process before starting another
      compilation-ask-about-save nil  ; save all buffers on `compile'
      compilation-scroll-output 'first-error)
    )
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
  :defer 3
  :hook
  ;; await until first input to activate it
  (pre-command . auto-revert-mode)
  ;; revert buffers when their files/state have changed
  ;; (focus-in . revert-buffer)
  ;; (after-save . revert-buffer)
  ;; (switch-buffer . revert-buffer)
  ;; (switch-window . revert-buffer)
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
  :ensure nil
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
  :diminish
  :config
  (prog-mode . vi-tilde-fringe-mode)
  (text-mode . vi-tilde-fringe-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Terminal and Shell Related Settings

;; *********************************
;; shell script mode

(use-package sh-script
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
  (define-key evil-normal-state-map (kbd "SPC !") 'shell-pop)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI RELATED PACKAGES

;; *********************************
;; Centaur tabs

(use-package centaur-tabs
  :disabled
  :if (version<= "27.1" emacs-version)
  :after evil
  :hook
  (window-setup . centaur-tabs-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ;; ("C-x p" . centaur-tabs-counsel-switch-group)
  (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward))
  :init
  (setq centaur-tabs-set-bar 'under) ;; display an underline over the selected tab:
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-modified-marker t) ;; display a marker indicating that a buffer has been modified (atom-style)
  (setq centaur-tabs-modified-marker " ● ")
  (setq centaur-tabs-close-button " × ")
  (setq centaur-tabs-cycle-scope 'tabs) ;; dont change tabs groups, cicle through
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-set-icons t) ;; use icons from all the icons
  (setq centaur-tabs-show-navigation-buttons t) ;; display cool navigations buttons
  :config
  ;; (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (when (member "Arial" (font-family-list))
    (centaur-tabs-change-fonts "Arial" 130)))



;; *********************************
;; Solaire mode

(use-package solaire-mode
  :disabled
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (eval-when-compile
    (solaire-global-mode +1)
    ;;(solaire-mode-swap-bg)
    )
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
  :hook (text-mode . goto-address-mode)
  :hook (prog-mode . goto-address-prog-mode)
  :config
  (define-key goto-address-highlight-keymap (kbd "RET") #'goto-address-at-point)
  )


;; *********************************
;; restart emacs

(use-package restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; WEB DEVELOPMENT MAJOR MODES

;; *********************************
;;
;; Typescript Mode

(use-package typescript-mode
  :mode
  (("\\.ts\\'" . typescript-mode)
    ("\\.tsx\\'" . typescript-mode))
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
(define-key evil-normal-state-map (kbd "SPC a c") #'angular-open-counterpart)


;; *********************************
;;
;; Web-Mode

(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
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
  :mode "\\.css\\'"
  :preface
  :hook
  (css-mode . emmet-mode)
  (css-mode . editorconfig-mode)
  ;; :init
  ;; (setq css-indent-offset 2)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; WEB DEVELOPMENT MINOR MODES


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
  )


;; *********************************
;;
;; ** PrettierJS

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
;; ** Emmet

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LANGUAGES SETUP

;; *********************************
;; go mode


(use-package go-mode
  :commands emacs-lisp-mode
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
  (setq lisp-indent-function 'common-lisp-indent-function)
  (setq-default lisp-indent-offset 2)
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; VERSION CONTROL

;; *********************************
;; magit

(use-package magit
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
  (eval-when-compile
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
    (evil-define-key evil-magit-state magit-stashes-section-map "K" 'magit-stash-clear))
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
  (:map evil-normal-state-map
    ("SPC g t" . git-timemachine-toggle))
  )

;; *********************************
;;
;; diff-hl (highlights uncommited diffs in the fringe)

(use-package diff-hl
  :defer 1
  :custom-face
  ;; Better looking colours for diff indicators
  (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  (diff-hl-change ((t (:background "#007ad3" :foreground "#ffffff"))))
  (diff-hl-insert ((t (:background "#00ff00" :foreground "#000000"))))
  (diff-hl-delete ((t (:background "#ff3300" :foreground "#ffffff"))))
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (eval-when-compile
    (global-diff-hl-mode)
    (diff-hl-flydiff-mode) ;; highlighting changes on the fly
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
  )

;; Mode for .gitignore files.
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; UI -> Modeline

(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode)
  ;; :init
  ;; (doom-modeline-mode 1)
  )

;; **************************************
;; hide-mode-line-mode

(use-package hide-mode-line
  :hook
  (Man-mode-hook . hide-mode-line-mode)
  (completion-list-mode-hook . hide-mode-line-mode)
  )


;; **************************************
;; my personal modeline

(use-package tau-modeline
  :ensure nil
  :init
  (doom-modeline-mode 1)
  )

;; ##################################################
;;; Emacs startup profiler

(use-package esup
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  ;; :commands is Only needed for functions without an autoload comment (;;;###autoload).
  :commands (esup))


;; trying this in the last part of the config

(use-package gcmh
  :demand t
  :init
  ;; (setq gcmh-verbose             t
  ;;       gcmh-lows-cons-threshold #x800000
  ;;       gcmh-high-cons-threshold most-positive-fixnum
  ;;       gcmh-idle-delay          3600)
  :config
  (gcmh-mode 1)
  )
