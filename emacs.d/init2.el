;; -*- outshine-startup-folded-p: t; eval: (outshine-mode); -*-
;;; package.el
;;; Commentary: personal init.el
;;; Code:

;; **************************************************
;; if emacs version is bellow 26.1, throw an error
(when (version< emacs-version "26.1")
  (error "Detected Emacs %s.  Doom only supports Emacs 26.1 and higher"
    emacs-version))

;; **************************************************
;;
;;; * Garbage Collection

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.

(setq gc-cons-threshold most-positive-fixnum)

;; **************************************************
;;
;;; * CONSTANTS AND VARIABLES

(defvar interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defvar debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Doom will log more.")

;; Systems
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;;; Directories/files
(defconst emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory.  Must end with a slash.")

(defconst config-dir
  (eval-when-compile (concat emacs-dir "config/"))
  "The path to the currently loaded .emacs.d directory.  Must end with a slash.")

(defconst packages-config-dir (concat config-dir "packages/")
  "Directory for package settings.")

(defconst cache-dir (concat emacs-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files.  Must end with a slash.")

(defconst env-file (concat config-dir "env")
  "The location of the envvar file.")

(defconst secrets-dir (concat config-dir "secrets/")
  "The location of secrets.")

;; Folder to save session
(defconst tau-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; **************************************************
;; * Configuration variables

;; These are values that are used throuhgout this configuration
;; Put here for convenient editing


(defvar tau/erc-nick               "tau"        "The ERC nick to use.")
(defvar tau/erc-password           nil        "The ERC password to use.")
(defvar tau/erc-port               nil        "The ERC port to use.")
(defvar tau/erc-server             nil        "The ERC server to use.")
;; Font sizes
(defvar font/default            "Courier"  "The font to use.")
(defvar font/hack            "Hack"  "The font to use.")
(defvar font-size/default      110        "The font size to use for default text.")
(defvar font-size/header-line  80        "The font size to use for the header-line.")
(defvar font-size/mode-line    100        "The font size to use for the mode-line.")
(defvar font-size/small        100        "The font size to use for smaller text.")
(defvar font-size/title        140        "The font size to use for titles.")

;; Indent sizes
(defvar indent/js                2        "The font size to use for titles.")
(defvar indent/html              4        "The font size to use for titles.")

;; Colors
(defvar color/vscode-status-bar      "#007ad3"        "The font size to use for titles.")

;; Modeline
(defvar color/modeline-active-bg      "#007af0"        "The font size to use for titles.")
(defvar color/modeline-active-fg      "#ffffff"        "The font size to use for titles.")
(defvar color/modeline-inactive-bg      "#007ad3"        "The font size to use for titles.")
(defvar color/modeline-inactive-fg      "#eeeeee"        "The font size to use for titles.")

;; Init time start
(defvar my-init-el-start-time (current-time) "Time when init.el was started.")

;; Set the used theme
(setq my-theme 'vscode-default-dark)


;; **************************************************
;;
;;; * PACKAGE MANAGEMENT

;;
;;; ** package.el

(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa/"))
(setq package-gnupghome-dir (expand-file-name "gpg" package-user-dir))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(package-initialize)

;;
;;; ** use-package

;; install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

;; load use-package
(eval-when-compile
  (require 'use-package))

;; Set `:ensure t` globally for all packages using use-package

(use-package use-package-ensure
  :config
  (setq use-package-always-ensure nil)
  )

;; Auto update packages

(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7) ;; in days
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

;; **************************************************
;;
;;; * Personal information

(setq user-full-name "Gustavo P Borges")
(setq user-mail-address "gugutz@gmail.com")
(setq work-mail-address "gugutz@stairs.studio")
(setq gmail-address "gugutz@gmail.com")
(setq nickname "gugutz")

;; my secrets
(let ((secret.el (expand-file-name ".secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))
;; (load-library "~/dotfiles/emacs.d/secrets.el.gpg")


;; **************************************************
;;
;;; * Emacs Startup

;; taken from `doom-emacs'
;;
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)


(add-to-list 'load-path "~/.emacs.d/packages/gcmh")
(use-package gcmh
  :diminish
  :config
  (gcmh-mode 1)
  )

;; **************************************************

;; * Check version (taken from memacs)
;; this uses emacs 26.1 feature 'early-init' to set a few things before init.el is loaded

;; CheckVer
(cond ((version< emacs-version "26.1")
        (warn "Emacs version bellow 26.1. Some features might not work well."))
  ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
           (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
           (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
     (and (version< emacs-version "27")
       (or (not (file-exists-p early-init-do-not-edit-f))
         (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
     (make-directory early-init-do-not-edit-d t)
     (copy-file early-init-f early-init-do-not-edit-f t t t t)
     (add-to-list 'load-path early-init-do-not-edit-d)
     (require 'early-init))))
;; -CheckVer


;; **************************************************
;;
;; * CORE SETTINGS

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; Except for the clipboard on Windows, where its contents could be in an
;; encoding that's wider than utf-8, so we let Emacs/the OS decide what encoding
;; to use.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
;; to, it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message user-login-name
  inhibit-default-init t
  initial-major-mode 'fundamental-mode
  initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;; Emacs is a huge security vulnerability, what with all the dependencies it
;; pulls in from all corners of the globe. Let's at least try to be more
;; discerning.
(setq gnutls-verify-error (getenv "INSECURE")
  tls-checktrust gnutls-verify-error
  tls-program '("gnutls-cli --x509cafile %t -p %p %h"
                 ;; compatibility fallbacks
                 "gnutls-cli -p %p %h"
                 "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Emacs stores authinfo in HOME and in plaintext. Let's not do that, mkay? This
;; file usually stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list (expand-file-name "authinfo.gpg" secrets-dir)
                     "~/.authinfo.gpg"))

;; Emacs on Windows frequently confuses HOME (C:\Users\<NAME>) and APPDATA,
;; causing `abbreviate-home-dir' to produce incorrect paths.
(when IS-WINDOWS
  (setq abbreviated-home-dir "\\`'"))

;; Don't litter `doom-emacs-dir'. We don't use `no-littering' because it's a
;; mote too opinionated for our needs.
(setq abbrev-file-name             (concat packages-config-dir "abbrev.el")
  async-byte-compile-log-file  (concat packages-config-dir "async-bytecomp.log")
  bookmark-default-file        (concat packages-config-dir "bookmarks")
  custom-file                  (concat config-dir "custom.el")
  custom-theme-directory       (concat emacs-dir "themes/")
  desktop-dirname              (concat packages-config-dir "desktop")
  desktop-base-file-name       "autosave"
  desktop-base-lock-name       "autosave-lock"
  pcache-directory             (concat cache-dir "pcache/")
  request-storage-directory    (concat cache-dir "request")
  server-auth-dir              (concat cache-dir "server/")
  shared-game-score-directory  (concat config-dir "shared-game-score/")
  tramp-auto-save-directory    (concat cache-dir "tramp-auto-save/")
  tramp-backup-directory-alist backup-directory-alist
  tramp-persistency-file-name  (concat cache-dir "tramp-persistency.el")
  url-cache-directory          (concat cache-dir "url/")
  url-configuration-directory  (concat packages-config-dir "url/")
  gamegrid-user-score-file-directory (concat packages-config-dir "games/"))

;; HACK Stop sessions from littering the user directory
;; (defadvice doom--use-cache-dir-a (session-id)
;;   :override #'emacs-session-filename
;;   (concat cache-dir "emacs-session." session-id))


;; **************************************************
;;
;;; * Optimizations

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

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when IS-WINDOWS
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
  ;; been determined.
  (setq inhibit-compacting-font-caches t))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(when interactive-mode
  (add-hook 'pre-command-hook (gcmh-mode +1))
  (with-eval-after-load 'gcmh
    (setq gcmh-idle-delay 10
      gcmh-verbose debug-mode)
    (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)))

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason. Disabling it completely could have many side-effects, so we
;;      defer it until later.
(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
    (defun doom-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))


;;
;;; MODE-local-vars-hook

;; File+dir local variables are initialized after the major mode and its hooks
;; have run. If you want hook functions to be aware of these customizations, add
;; them to MODE-local-vars-hook instead.
(defun doom-run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are initialized."
  (run-hook-wrapped (intern-soft (format "%s-local-vars-hook" major-mode))
    #'doom-try-run-hook))
(add-hook 'hack-local-variables-hook #'doom-run-local-var-hooks-h)

;; If the user has disabled `enable-local-variables', then
;; `hack-local-variables-hook' is never triggered, so we trigger it at the end
;; of `after-change-major-mode-hook':
(defun doom-run-local-var-hooks-if-necessary-h ()
  "Run `doom-run-local-var-hooks-h' if `enable-local-variables' is disabled."
  (unless enable-local-variables
    (doom-run-local-var-hooks-h)))
(add-hook 'after-change-major-mode-hook
  #'doom-run-local-var-hooks-if-necessary-h
  'append)

;; **************************************************
;;
;; * General editor settings

;; set frame title
(setq frame-title-format '("Emacs"))

;; Allow access from emacsclient
(use-package server
  :when (display-graphic-p)
  :defer 1
  :config
  (unless (or (daemonp) (server-running-p))
    (server-start))
  )

;; Add the folder 'config' to emacs load-path so i can require stuff from there
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
;; (add-to-list 'load-path "~/dotfiles/emacs.d/config")

;; indicate empty lines in the fringe with --
(setq-default indicate-empty-lines t)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
  split-height-threshold nil)

;; **************************************************
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

;; **************************************************
;;
;;; cursor

;; dont blink the cursor
(blink-cursor-mode 0)
(setq blink-cursor-blinks 0) ;; blink forever

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

(setq create-lockfiles nil) ;; Prevent emacs to create lockfiles (.#files#). this also stops preventing editing colisions, so watch out

(setq make-backup-files nil) ;; dont make backup files

(setq confirm-kill-processes nil) ;; dont ask confirmation to kill processes

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
  emacs-tmp-dir)


;; **************************************************
;;
  ;;; File Handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
  vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

(setq vc-follow-symlinks t) ;; Always follow symbolic links to edit the 'actual' file it points to


(setq confirm-kill-processes nil) ;; dont ask confirmation to kill processes

;; Disable the annoying Emacs bell ring (beep)
(setq ring-bell-function 'ignore)

;; Create alias to yes-or-no anwsers (y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)


(setq large-file-warning-threshold nil) ;; Don’t warn me about opening large files
(setq next-line-add-newlines t) ;; C-n insert newlines if the point is at the end of the buffer.
(setq large-file-warning-threshold 100000000) ;; warn when opening files bigger than 100MB


;; ** C-k kills current buffer without having to select which buffer
;; By default C-x k prompts to select which buffer should be selected.
;; I almost always want to kill the current buffer, so this snippet helps in that.

;; Kill current buffer; prompt only if
;; there are unsaved changes.
(global-set-key (kbd "C-x k")
  '(lambda () (interactive) (kill-buffer (current-buffer)))
  )

(setq require-final-newline t)  ;; add final newline to files


;; **************************************************
;;
;;; Formatting

;; Indentation
(setq-default tab-width 4
  tab-always-indent t
  indent-tabs-mode nil
  fill-column 80) ;; Sets a 80 character line width


;; Word wrapping
(setq-default word-wrap t
  truncate-lines t
  truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
  delete-trailing-lines nil
  require-final-newline t
  tabify-regexp "^\t* [ \t]+")  ; for :retab

;; Favor hard-wrapping in text modes
(add-hook 'text-mode-hook #'auto-fill-mode)

;; **************************************************
;;
;;; Clipboard / kill-ring

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

;;
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Fixes the clipboard in tty Emacs by piping clipboard I/O through xclip, xsel,
;; pb{copy,paste}, wl-copy, termux-clipboard-get, or getclip (cygwin).
(unless (eq system-type 'windows-nt)
  (add-hook 'tty-setup-hook
    (defun doom-init-clipboard-in-tty-emacs-h ()
      (and (not (getenv "SSH_CONNECTION"))
        (require 'xclip nil t)
        (xclip-mode +1)))))


;; **************************************************
;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)

;; **************************************************
;;
;; ** preffer UTF-8 coding system

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)




;; auto balance windows on opening and closing frames
(setq window-combination-resize t)

;; ** set default line spacing
;; (setq-default line-spacing 1) ;; A nice line height
(setq-default line-spacing 2)

;; fix wierd color escape system
(setq system-uses-terminfo nil) ;; Fix weird color escape sequences

;; confirm before closing emacs
;; (setq confirm-kill-emacs 'yes-or-no-p) ;; Ask for confirmation before closing emacs

;; select window for help
(setq help-window-select t)

;; delete selection mode

;; Delete Selection mode lets you treat an Emacs region much like a typical text selection outside of Emacs: You can replace the active region just by typing text, and you can delete the selected text just by hitting the Backspace key (‘DEL’).

;; According to the Emacs manual,
;; If you enable Delete Selection mode, a minor mode, then inserting text while the mark is active causes the selected text to be deleted first. This also deactivates the mark. Many graphical applications follow this convention, but Emacs does not.


(delete-selection-mode 1)


;; **************************************************

;; * PACKAGES

;;;###package image
(setq image-animate-loop t)

;;;###pos tip
(setq pos-tip-internal-border-width 6
  pos-tip-border-width 1)
;; ** diminish

(use-package diminish
  :ensure t
  )

;; ** outshine mode


(use-package outshine
  :ensure t
  :commands outshine-mode
  :diminish
  :init
  ;; To enable the keybindings, you must set the variable outline-minor-mode-prefix (note the variable name carefully) before loading Outshine, e.g.:
  (defvar outline-minor-mode-prefix "\M-#")
  :hook
  (emacs-lisp-mode . outshine-mode)
  )

;; ** outline mode
;; this is only here so i can diminish it
(use-package outline
  :ensure nil
  :diminish
  )

;; ** visual-line-mode (word wrap)
(use-package visual-line-mode
  :ensure nil
  :diminish
  :hook
  (prog-mode . turn-on-visual-line-mode)
  (text-mode . turn-on-visual-line-mode)
  )

;; **************************************************

;; * TEXT MANIPULATION

;;------------------------------
;;
;;; Use the system clipboard
(setq x-select-enable-clipboard t)

;; Helper functions for casing words


(defun upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg))
  )
(global-set-key (kbd "C-M-u")	 'upcase-backward-word)

(defun downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg))
  )
(global-set-key (kbd "C-M-l")	 'downcase-backward-WORD)

(defun capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg))
  )
;; this replaces native capitlize word!
(global-set-key (kbd "C-M-c")	 'capitalize-backward-word)


;;------------------------------
;;
;;; Copy to system clipboard

(defun copy-to-clipboard ()
  "Make F8 and F9 Copy and Paste to/from OS Clipboard.  Super usefull."
  (interactive)
  (if (display-graphic-p)
    (progn
      (message "Yanked region to x-clipboard!")
      (call-interactively 'clipboard-kill-ring-save)
      )
    (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )
(global-set-key [f9] 'copy-to-clipboard)

;; ------------------------------
;;
;;; Paste

(defun paste-from-clipboard()
  "Allow to paste from system clipboard."
  (if (display-graphic-p)
    (progn
      (clipboard-yank)
      (message "graphics active")
      )
    (insert (shell-command-to-string "xsel -o -b")) ) )

(global-set-key [f10] 'paste-from-clipboard)


;; **************************************************

;; ** display-line-numbers

(use-package display-line-numbers
  :if (version<= "26.0.50" emacs-version)
  :ensure nil
  :config
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  (setq linum-format "%4d \u2502 ") ; 4 chars and a space with solid line separator
  ;; Explicitly define a width to reduce computation
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions makes it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Enable line numbers in most text-editing modes. We avoid
  ;; `global-display-line-numbers-mode' because there are many special and
  ;; temporary modes where we don't need/want them.
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)

  ;; Select lines by click-dragging on the margin. Tested with GNU Emacs 23.3
  (defvar *linum-mdown-line* nil)
  (defun line-at-click ()
    (save-excursion
      (let ((click-y (cdr (cdr (mouse-position))))
             (line-move-visual-store line-move-visual))
        (setq line-move-visual t)
        (goto-char (window-start))
        (next-line (1- click-y))
        (setq line-move-visual line-move-visual-store)
        ;; If you are using tabbar substitute the next line with
        (line-number-at-pos))))

  (defun md-select-linum ()
    (interactive)
    (goto-line (line-at-click))
    (set-mark (point))
    (setq *linum-mdown-line*
      (line-number-at-pos)))

  (defun mu-select-linum ()
    (interactive)
    (when *linum-mdown-line*
      (let (mu-line)
        ;; (goto-line (line-at-click))
        (setq mu-line (line-at-click))
        (goto-line (max *linum-mdown-line* mu-line))
        (set-mark (line-end-position))
        (goto-line (min *linum-mdown-line* mu-line))
        (setq *linum-mdown*
          nil))))

  (global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
  (global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
  (global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)
  )

;; **************************************************
;; * GPG Encryption

(use-package epa-file
  :config
  (epa-file-enable)
  (setq epa-file-encrypt-to '("gugutz@gmail.com"))

  ;; Control whether or not to pop up the key selection dialog.
  (setq epa-file-select-keys 0)
  ;; Cache passphrase for symmetric encryption.
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  )

;; **************************************************

;; ** Turn on auto-revert mode (auto updates files changed on disk)

(use-package autorevert
  :ensure nil
  :hook
  ;; revert buffers when their files/state have changed
  (focus-in . revert-buffer)
  (after-save . revert-buffer)
  (switch-buffer . revert-buffer)
  (switch-window . revert-buffer)
  :preface
  :config
  (setq auto-revert-verbose nil) ; let us know when it happens
  (setq auto-revert-use-notify nil)
  (setq auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (setq revert-without-query (list "."))
  (setq auto-revert-interval 1) ;; rever every 1 second
  (setq auto-revert-check-vc-info t)

  )

;; refresh buffer with F5
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))


;; ** Remove the ^M characters from files that contains Unix and DOS line endings
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  )

;; *** Hook it to text-mode and prog-mode
(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'prog-mode-hook 'remove-dos-eol)


;; ** expand-region

;; smart selection of text

(use-package expand-region
  :ensure t
  :defer t
  :bind
  ([(control shift iso-lefttab)] . 'er/expand-region)
  )


;; * Code editing settings
;; ** subword-mode

;; : Alt+x subword-mode. It change all cursor movement/edit commands to stop in-between the “camelCase” words.
;; : subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.


(use-package subword
  :ensure nil
  :hook
  (clojure-mode . subword-mode)
  (ruby-mode . subword-mode)
  (enh-ruby-mode . subword-mode)
  (elixir-mode . subword-mode)
  )


;; ** superword-mode

;; : Alt+x superword-mode (emacs 24.4) is similar. It treats text like “x_y” as one word. Useful for “snake_case”.
;; : subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.


(use-package superword
  :ensure nil
  :hook
  (js2-mode . superword-mode)
  )


;; ** default indentation

(setq-default indent-tabs-mode nil)
;; C e C-like langs default indent size
(setq-default tab-width 2)
;; Perl default indent size
(setq-default cperl-basic-offset 2)
(setq-default c-basic-offset 2)


;; ** Use unix-conf-mode for .*rc files

  (use-package conf-mode
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
  ;; (add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))


  ;; ** iedit

  (use-package iedit
    :config
    (set-face-background 'iedit-occurrence "Magenta")
    :bind
    ("C-;" . iedit-mode)
    )


  ;; ** eldoc
  ;; Enable documentation for programming languages


  (use-package eldoc
    :ensure nil
    :diminish
    :hook
    (prog-mode . eldoc-mode)
    ;;(prog-mode       . turn-on-eldoc-mode)
    ;; (cider-repl-mode . turn-on-eldoc-mode)
    :config
    ;; (global-eldoc-mode -1)
    ;; (add-hook 'prog-mode-hook 'eldoc-mode)
    (setq eldoc-idle-delay 0.4)
    )


  ;; *** eldoc-box

  ;; Show eldoc info in a childframe


  (use-package eldoc-box
    :ensure t
    :after eldoc
    :custom-face
    ;;(eldoc-box-border (t (:background "#202020"))))
    ;;(eldoc-box-body (t (:background "#202020"))))
    :config
    ;;(setq eldoc-box-max-pixel-width)
    ;;(setq eldoc-box-max-pixel-height)
    (setq eldoc-box-only-multi-line nil)   ;;  Set this to non-nil and eldoc-box only display multi-line message in childframe. One line messages are left in minibuffer.
    ;; (eldoc-box-hover-mode)
    (eldoc-box-hover-at-point-mode)
    )


  ;; ** aggressive-indent-mode

  (use-package aggressive-indent
    :ensure t
    :defer t
    :custom
    (aggressive-indent-comments-too t)
    :hook
    (emacs-lisp-mode . aggressive-indent-mode)
    :config
    )


  ;; ** interactive-align

  ;; Keymap used in the minibuffer when ialign command is executed.
  ;; |---------+--------------------------|
  ;; | Key     | Command                  |
  ;; |---------+--------------------------|
  ;; | C-c C-r | ialign-toggle-repeat     |
  ;; | C-c C-t | ialign-toggle-tabs       |
  ;; | C-c M-c | ialign-toggle-case-fold  |
  ;; | C-c +   | ialign-increment-spacing |
  ;; | C-c -   | ialign-decrement-spacing |
  ;; | C-c [   | ialign-decrement-group   |
  ;; | C-c ]   | ialign-increment-group   |
  ;; | C-c C-f | ialign-set-group         |
  ;; | C-c C-s | ialign-set-spacing       |
  ;; | C-c RET | ialign-commit            |
  ;; | C-c C-c | ialign-update            |
  ;; | C-c ?   | ialign-show-help         |
  ;; |---------+--------------------------|


  (use-package ialign
    :ensure t
    :bind
    ("C-x l" . ialign)
    :config
    ;;(setq ialign-default-spacing 32)
    (setq ialign-align-with-tabs nil) ;; default nil
    (setq ialign-auto-update t) ;; default t
    )


  ;; ** align.el

  ;; align text to a specific column, by regexp

  ;; This mode allows you to align regions in a context-sensitive fashion.
  ;; The classic use is to align assignments:

  ;; int a = 1;
  ;; short foo = 2;
  ;; double blah = 4;

  ;; becomes

  ;; int    a    = 1;
  ;; short  foo  = 2;
  ;; double blah = 4;


  (defun align-values (start end)
    "Vertically aligns region based on lengths of the first value of each line.
Example output:

	foo        bar
	foofoo     bar
	foofoofoo  bar"
    (interactive "r")
    (align-regexp start end
      "\\S-+\\(\\s-+\\)"
      1 1 nil))


;; ** better-jumper
(use-package better-jumper
  :ensure t
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (add-hook 'better-jumper-post-jump-hook #'recenter)
  )

;; ** dtrt

(use-package dtrt-indent
  ;; Automatic detection of indent settings
  :defer t
  :config
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)
  )
;; ** align-regexp


;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)


;; ** dumb-jump
;; Emacs jump to definition tool


(use-package dumb-jump
  :ensure t
  :preface
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
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
  :custom
  (dumb-jump-selector 'ivy)
  :config
  (eval-when-compile
    (require 'helm-source nil t))
  )


;; * Misc Packages

;; ** helpfull

;; A better replacement for emacs help system


(use-package helpful
  :ensure t
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


;; **************************************************

(use-package lisp-mode
  :commands emacs-lisp-mode
  :preface
  (defun setup-elisp-mode ()
    (interactive)
    (message "Trying to setup elisp-mode for buffer")
    (flycheck-mode 1)
    (eldoc-mode 1)
    (yas-minor-mode 1)
    (add-node-modules-path)
    (editorconfig-mode 1)
    (dumb-jump-mode 1)
    (hl-todo-mode 1)
    (smartscan-mode 1)
    )
  :hook
  (emacs-lisp-mode . setup-elisp-mode)
  (lisp-interaction-mode . eldoc-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  (bind-key "C-c c" 'compile emacs-lisp-mode-map)
  )

;; ** undo-tree

(use-package undo-tree
  :ensure t
  :diminish
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
    `(("." . ,(concat cache-dir "undo-tree-hist/"))))

  ;; ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; ;; file savings (these files add up over time and zstd is so incredibly fast).
  ;; (when (executable-find "zstd")
  ;;   (defadvice doom--undo-tree-make-history-save-file-name-a (file)
  ;;     :filter-return #'undo-tree-make-history-save-file-name
  ;;     (concat file ".zst")))

  ;; ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  ;; (defadvice doom--undo-tree-strip-text-properties-a (&rest _)
  ;;   :before #'undo-list-transfer-to-tree
  ;;   (dolist (item buffer-undo-list)
  ;;     (and (consp item)
  ;;       (stringp (car item))
  ;;       (setcar item (substring-no-properties (car item))))))

  ;; ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; ;; in the echo-area.
  ;; (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)

  (global-undo-tree-mode +1)

  )


;; ** restclient

(use-package restclient
  :ensure t
  :mode
  ("\\.rest$\\'" "\\.http$\\'")
  :config
  (setq restclient-same-buffer-response t)
  (progn
    ;; Add hook to override C-c C-c in this mode to stay in window
    (add-hook 'restclient-mode-hook
      '(lambda ()
         (local-set-key
           (kbd "C-c C-c")
           'restclient-http-send-current-stay-in-window))))
  )


;; ** ob-restclient

;; Org source blocks and exporter for restclient


(use-package ob-restclient
  :ensure t
  :mode "\\.rest$"
  :config
  ;; add restclient to org-babel languages
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((restclient . t)))
  )


;; **************************************************

;; ** exec-path-from-shell

;; Make emacs use $PATH defined in the systems shell

;; : snippet taken from oficial use package github page

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :init
  ;;(setenv "SHELL" "/bin/zsh")
  ;;(setq explicit-shell-file-name "/bin/zsh")
  ;;(setq shell-file-name "zsh")
  :config
  ;; This sets $MANPATH, $PATH and exec-path from your shell, but only on OS X and Linux.
  (exec-path-from-shell-initialize)
  ;; Its possible to copy values from other SHELL variables using one of the two methods bellow
  ;; either using the `exec-path-from-shell-copy-env' functon or setting the variable `exec-path-from-shell-variables'
  ;; (exec-path-from-shell-copy-env "PYTHONPATH")
  ;; (setq exec-path-from-shell-variables '("PYTHONPATH" "GOPATH"))
  )


;; ** ws butler
;; Delete trailing whitespace on save

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package ws-butler
  :ensure t
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :config
  (ws-butler-global-mode +1)
  )


;; * Spellchecking


(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer


;; ** Flyspell

;; Change dictionaries with F12


                                        ;(defun fd-switch-dictionary()
                                        ;(interactive)
                                        ;(let* ((dic ispell-current-dictionary)
                                        ;    (change (if (string= dic "deutsch8") "english" "deutsch8")))
                                        ;  (ispell-change-dictionary change)
                                        ;  (message "Dictionary switched from %s to %s" dic change)
                                        ;  ))

                                        ;(global-set-key (kbd "<f12>")   'fd-switch-dictionary)



;; Change dictionaries with F12 (teste pt-br)
(let ((langs '("american" "brasileiro")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem))
  )

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang))
  )

(global-set-key (kbd "<f12>")   'cycle-ispell-languages)



(use-package flyspell
  :defer 1
  :disabled
  :hook
  (text-mode . flyspell-mode)
  :config
  ;; ignore org source blocks from spellchecking
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^"))

  ;; global ispell settings (disabled in favor of conditional hunspell setup bellow)
  ;; (setenv "LANG" "en_US.UTF-8")
  ;; (setq ispell-program-name "aspell")
  ;; (setq ispell-program-name "hunspell")
  ;; (setq ispell-dictionary "en_US")
  ;; (setq ispell-local-dictionary "pt_BR")
  ;; (setq ispell-local-dictionary "en_US")

  ;; Hunspell settings
  ;; find aspell and hunspell automatically
  ;;  (cond
  ;;    ;; try aspell first in case both aspell and hunspell are installed, it will
  ;;    ;; set `ispell-program-name' to use hunspell
  ;;    ((executable-find "aspell")
  ;;      (setq ispell-program-name "aspell")
  ;;      ;; Please note `ispell-extra-args' contains ACTUAL parameters passed to aspell
  ;;      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  ;;      ;;(setq ispell-local-dictionary "pt_BR")
  ;;    )
  ;;   ;; if hunspell is available, use it instead of aspell for multilang support
  ;;    ((executable-find "hunspell")
  ;;      (setq ispell-program-name "hunspell")
  ;;      ;; i could set `ispell-dictionary' instead but `ispell-local-dictionary' has higher priority
  ;;      (setq ispell-local-dictionary "en_US")
  ;;      ;; setup both en_US and pt_BR dictionaries in hunspell
  ;;      (ispell-hunspell-add-multi-dic "en_US,pt_BR")
  ;;
  ;;      (setq ispell-local-dictionary-alist
  ;;         ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
  ;;         ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
  ;;         '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,pt_BR") nil utf-8))
  ;;      )
  ;;    )
  ;;  )

  )


;; ** guess-language

;; Automatic guess the language of the paragraph im writing in
;; Works with mutilang documents


(use-package guess-language         ; Automatically detect language for Flyspell
  :ensure t
  :disabled
  :defer t
  :hook
  (text-mode . guess-language-mode)
  ;; :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_US" "English"))
                                    (pt . ("pt_BR" "Portuguese Brazilian"))))
  (setq guess-language-languages '(en pt))
  (setq guess-language-min-paragraph-length 45)
  )



;; ** move-text


(use-package move-text
  :ensure t
  :after evil
  :bind
  ([(meta k)] . move-text-up)
  ([(meta j)] . move-text-down)
  ([(meta shift k)] . move-text-line-up)
  ([(meta shift j)] . move-text-line-down)
  ([(meta shift up)] . move-text-up)
  ([(meta shift down)] . move-text-down)
  :init
  ;; free the bindings used by this plugin from windmove and other areas that use the same keys
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-k"))
  (global-unset-key (kbd "C-S-j"))
  (global-unset-key (kbd "C-S-k"))
  :config
  (move-text-default-bindings)
  ;; tried setting these in :bind but use package executes :bind along with init, and i needed to free the keys before
  (define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
  (define-key evil-normal-state-map (kbd "M-k") 'move-text-up)
  (define-key evil-visual-state-map (kbd "M-j") 'move-text-region-up)
  (define-key evil-visual-state-map (kbd "M-k") 'move-text-region-down)
  )



;; * Mouse configuration
;; ** Enable mouse support in terminal mode


(when (eq window-system nil)
  (xterm-mouse-mode 1))



;; (use-package mouse3
;;     :config
;; (global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu))


;; ** right-click-context-menu


(use-package right-click-context
  :ensure t
  :config
  (global-set-key (kbd "<menu>") 'right-click-context-menu)
  (global-set-key (kbd "<mouse-3>") 'right-click-context-menu)
  (bind-key "C-c <mouse-3>" 'right-click-context-menu)

  ;; (setq right-click-context-mode-lighter "🐭")

  ;; customize the right-click-context-menu
  (let ((right-click-context-local-menu-tree
          (append right-click-context-global-menu-tree
            '(("Insert"
                ("Go to definition" :call (lsp-goto-type-definition)
                  ("FooBar" :call (insert "FooBar"))
                  )))))
         (right-click-context-menu)))
  )


;; ** zoom buffers with Mouse+Scroll<Up/Down> like in the browser


;; zoom in/out like we do everywhere else.

;; mouse scrolls are binded differently depending on the system
(if (eq system-type 'gnu/linux)
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
  )
(if (eq system-type 'windows-nt)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  )
(global-set-key [?\C-=] 'text-scale-increase)
(global-set-key [?\C--] 'text-scale-decrease)

;; * hippie-expand (native emacs expand function)


(use-package hippie-exp
  :ensure nil
  :defer t
  :bind
  ("<tab>" . hippie-expand)
  ("<C-return>" . hippie-expand)
  ("C-M-SPC" . hippie-expand)
  (:map evil-insert-state-map
    ("<tab>" . hippie-expand)
    )
  :config
  (setq-default hippie-expand-try-functions-list
    '(yas-hippie-try-expand
       company-indent-or-complete-common
       emmet-expand-yas
       emmet-expand-line
       indent-according-to-mode
       ))
  )


;; * Evil


(use-package evil
  :ensure t
  :custom
  (evil-ex-complete-emacs-commands nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-shift-round nil)
  (evil-esc-delay 0)  ;; Don't wait for any other keys after escape is pressed.
  ;; Make Evil look a bit more like (n) vim  (??)
  (evil-search-module 'isearch-regexp)
  ;; (setq evil-search-module 'evil-search)
  (evil-magic 'very-magic)
  (evil-shift-width (symbol-value 'tab-width))
  (evil-regexp-search t)
  (evil-search-wrap t)
  ;; (setq evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo nil)
  (evil-want-integration nil)
  ;; (setq evil-want-abbrev-on-insert-exit nil)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-mode-line-format '(before . mode-line-front-space)) ;; move evil tag to beginning of modeline
  ;; Cursor is alway black because of evil.
  ;; Here is the workaround
  ;; (@see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default)
  (evil-default-cursor t)
  ;; change cursor color according to mode
  (evil-emacs-state-cursor '("#ff0000" box))
  (evil-motion-state-cursor '("#FFFFFF" box))
  (evil-normal-state-cursor '("#00ff00" box))
  (evil-visual-state-cursor '("#abcdef" box))
  (evil-insert-state-cursor '("#e2f00f" bar))
  (evil-replace-state-cursor '("red" hbar))
  (evil-operator-state-cursor '("red" hollow))
  :bind
  (:map evil-normal-state-map
    (", w" . evil-window-vsplit)
    ("C-r" . undo-tree-redo))
  (:map evil-insert-state-map
    ;; this is also defined globally above in the config
    ("C-S-<backtab>" . er/expand-region))
  (:map evil-visual-state-map
    ;; this is also defined globally above in the config
    ("<tab>" . indent-region)
    ("C-/" . comment-line)
    ("C-S-/" . comment-region)
    ("C-S-M-/" . comment-box)
    ("M-=" . #'align-values))

  ;; check if global-set-key also maps to evil insert mode; if yes delete bellow snippets
  :config
  (evil-mode)
  ;; unset evil bindings that conflits with other stuff
  (define-key evil-insert-state-map (kbd "<tab>") nil)
  (define-key evil-normal-state-map (kbd "<tab>") nil)
  (define-key evil-visual-state-map (kbd "<tab>") nil)
  ;; use esc while in emacs state to return to normal evil mode
  (define-key evil-emacs-state-map [escape] 'evil-normal-state)
  ;; vim-like navigation with C-w hjkl
  (define-prefix-command 'evil-window-map)
  (define-key evil-window-map (kbd "h") 'evil-window-left)
  (define-key evil-window-map (kbd "j") 'evil-window-down)
  (define-key evil-window-map (kbd "k") 'evil-window-up)
  (define-key evil-window-map (kbd "l") 'evil-window-right)
  (define-key evil-window-map (kbd "b") 'evil-window-bottom-right)
  (define-key evil-window-map (kbd "c") 'evil-window-delete)
  (define-key evil-motion-state-map (kbd "M-w") 'evil-window-map)
  ;; make esc quit or cancel everything in Emacs
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'keyboard-quit)
  ;;-----------------------------------------
  ;; recover native emacs commands that are overriden by evil
  ;; this gives priority to native emacs behaviour rathen than Vim's
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-delete-char)
  (define-key evil-normal-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-visual-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
  (define-key evil-normal-state-map (kbd "C-w") 'kill-region)
  (define-key evil-visual-state-map (kbd "C-w") 'kill-region)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-insert-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-visual-state-map (kbd "C-w") 'evil-delete)
  (define-key evil-normal-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-visual-state-map (kbd "C-y") 'yank)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-visual-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-visual-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "Q") 'call-last-kbd-macro)
  (define-key evil-visual-state-map (kbd "Q") 'call-last-kbd-macro)
  (define-key evil-insert-state-map (kbd "C-r") 'search-backward)
  )



;; * Evil packages / plugins

;; ** Evil-ORG


(use-package evil-org
  :after org evil
  :hook
  (org-mode . evil-org-mode)
  :config
  (lambda ()
    (evil-org-set-key-theme))
  )


;; ** evil-numbers

i
1
(use-package evil-numbers
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
1    ("C-c C-+" . evil-numbers/inc-at-pt)
    ("C-c C--" . evil-numbers/dec-at-pt)
    ("<kp-add>" . evil-numbers/inc-at-pt)
    ("<kp-subtract>" . evil-numbers/dec-at-pt))
  :config
  (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)
  )

;; ** evil-visualstar

;; Allows `#' and `*' searches on visual-mode

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

;; ** evil-leader


(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ;;"e" 'find-file  ;; removed in favor of counsel-find-file
    "q" 'evil-quit
    "w" 'save-buffer
    "d" 'delete-frame
    "k" 'kill-buffer
    "b" 'ivy-switch-buffer
    "-" 'split-window-bellow
    "|" 'split-window-right
    "." 'find-tag
    "t" 'projectile-find-file
    "b" 'ivy-switch-buffer
    "vc" 'evilnc-comment-or-uncomment-lines
    "ag" 'projectile-ag
    "," 'switch-to-previous-buffer
    ;;counsel bindings
    "e" 'counsel-find-file
    "f" 'counsel-projectile-find-file
    "cg" 'counsel-ag
    "r" 'counsel-rg
                                        ; "gg" 'git-gutter+:toggle
                                        ; "gd" 'git-gutter+:popup-diff
                                        ; "gp" 'git-gutter+:previous-hunk
                                        ; "gn" 'git-gutter+:next-hunk
                                        ; "gr" 'git-gutter+:revert-hunk
    "gb" 'mo-git-blame-current
    "gL" 'magit-log
    "gs" 'magit-status
    "q"  'kill-buffer-and-window
    "u"  'undo-tree-visualize
    "nn" 'neotree-toggle
    "nm" 'next-match
    "nf" 'neotree-find
    ;; windmove bindings
    "gk" 'windmove-up
    "gj" 'windmove-down
    "gl" 'windmove-right
    "gh" 'windmove-left
    "vs" 'split-window-right
    "hs" 'split-window-below
    "s"  'ispell-word
    "ht" 'alchemist-help-search-at-point
    "gt" 'alchemist-goto-definition-at-point
    "mf" 'elixir-format
    "ll" 'longlines-mode
    "x" 'smex
  "|" 'split-window-right)
  )


;; ** Evil Surround


(use-package evil-surround
  :preface
  (defun evil-surround-prog-mode-hook-setup ()
    "Documentation string, idk, put something here later."
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

  :config
  (global-evil-surround-mode 1)
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)
  (add-hook 'js2-mode-hook 'evil-surround-js-mode-hook-setup)
  (add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)
  (add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)
  )

;; ** evil-commentary

;; gcc -> comments a line
;; gc -> comments the target of a motion (eg: gcap -> comment all paragraph)

(use-package evil-commentary
  :ensure t
  :diminish
  :config
  (evil-commentary-mode)
  )


;; ** Evil-Matchit

;; Press “%” to jump between matched tags in Emacs

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1)
  )


;; ** evil-paredit


(use-package evil-paredit
  :ensure t
  :defer t
  :hook
  (emacs-lisp-mode . evil-paredit-mode)
  )


;; ** evil-mc

;; Multiple cursors for evil mode

;; |------------+---------------------------------|
;; | Key        | action                          |
;; |------------+---------------------------------|
;; | C-t or grn | skip creating a cursor forward  |
;; | grp        | skip creating a cursor backward |
;; | gru        | undo last addded cursor         |
;; | grq        | remove all cursors              |
;; |------------+---------------------------------|


(use-package evil-mc
  :ensure t
  :defer t
  :after evil
  :bind
  (:map evil-visual-state-map
    ("C-d" . mc/mark-next-like-this)
    ("C-a" . mc/mark-all-like-this)
    ("C-a" . mc/mark-all-like-this)
    )
  (:map evil-visual-state-map
    ("C-d" . evil-mc-make-and-goto-next-match) ;; Make a cursor at point and go to the next match of the selected region or the symbol under cursor.
    ("C-a" . evil-mc-make-all-cursors) ;; Create cursors for all strings that match the selected region or the symbol under cursor.
    ("C-q" . evil-mc-undo-all-cursors)  ;; Remove all cursors.
    )
  :config
  (define-key evil-visual-state-map (kbd "mn") 'evil-mc-make-and-goto-next-match)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)
  (global-evil-mc-mode  1)
  )


;; ** evil-goggles

(use-package evil-goggles
  :ensure t
  :defer t
  :custom
  (evil-goggles-pulse t) ;; default is to pulse when running in a graphic display
  (evil-goggles-duration 0.200) ;; default is 0.200

  ;; list of all on/off variables, their default value is `t`:
  (evil-goggles-enable-paste nil) ;; to disable the hint when pasting
  (evil-goggles-enable-delete t)
  (evil-goggles-enable-change t)
  (evil-goggles-enable-indent t)
  (evil-goggles-enable-yank t)
  ;;(evil-goggles-enable-join t)
  ;;(evil-goggles-enable-fill-and-move t)
  (evil-goggles-enable-paste t)
  ;;(evil-goggles-enable-shift t)
  (evil-goggles-enable-surround t)
  (evil-goggles-enable-commentary t )
  ;;(evil-goggles-enable-nerd-commenter t)
  ;;(evil-goggles-enable-replace-with-register t)
  (evil-goggles-enable-set-marker t)
  (evil-goggles-enable-undo t)
  (evil-goggles-enable-redo t)
  ;;(evil-goggles-enable-record-macro t)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )

;; ** evil-lion

;; : Align by operators

;; Example, left align gl:
;; After pressing glip= (gl is the operator, ip text object paragraph, = separator)

;; one = 1
;; three = 3
;; fifteen = 15

;; will become:

;; one     = 1
;; three   = 3
;; fifteen = 15


(use-package evil-lion
  :ensure t
  :bind
  (:map evil-normal-state-map
    ("g l " . evil-lion-left)
    ("g L " . evil-lion-right)
    :map evil-visual-state-map
    ("g l " . evil-lion-left)
    ("g L " . evil-lion-right))
  :config
  (setq evil-lion-squeeze-spaces t) ;; default t
  (evil-lion-mode)
  )

;; **************************************************

;; ** evil-easymotion

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC")
  )

;; ** evil-escape

(use-package evil-escape
  :ensure t
  :diminish
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  ;; (global-set-key (kbd "C-c C-g") 'evil-escape)
  (global-set-key [escape] 'evil-escape)
  (global-set-key [remap keyboard-quit] 'evil-escape)
  )
;; * ORG-MODE

;; ** org-mode setup

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :defer t
  :preface
  (defun setup-org-mode ()
    (interactive)
    (message "Trying to setup org-mode for buffer")
    (color-identifiers-mode)
    (flycheck-mode 1)
    (rainbow-mode 1)
    (diff-hl-mode)
    (prettify-symbols-mode 1)
    (org-bullets-mode 1)
    )
  :hook
  (org-mode . setup-org-mode)
  :bind
  (:map org-mode-map
    ("C-c o l" . org-store-link)
    ("C-c o l" . org-store-link)
    ("C-c o a" . org-agenda)
    ("C-c o c" . org-capture)
    ("C-c o b" . org-switch))

  ;; this map is to delete de bellow commented lambda that does the same thing
  ;; Resolve issue with Tab not working with ORG only in Normal VI Mode in terminal
  ;; (something with TAB on terminals being related to C-i...)
  (:map evil-normal-state-map
    ("<tab>" . org-cycle))
  :init
  ;; general org config variables
  (setq org-log-done 'time)
  (setq org-export-backends (quote (ascii html icalendar latex md odt)))
  (setq org-use-speed-commands t)

  ;; dont display atual width for images inline. set per-file with
  ;; #+ATTR_HTML: :width 600px :height: auto
  ;; #+ATTR_ORG: :width 600
  ;; #+ATTR_LATEX: :width 5in
  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)

  ;; make tab behave like it usually do (ie: indent) inside org source blocks
  (setq org-src-tab-acts-natively t)

  (setq org-confirm-babel-evaluate 'nil)
  (setq org-todo-keywords
    '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  (setq org-agenda-window-setup 'other-window)
  (setq org-log-done 'time) ;; Show CLOSED tag line in closed TODO items
  (setq org-log-done 'note) ;; Prompt to leave a note when closing an item
  (setq org-hide-emphasis-markers nil)

  ;;ox-twbs (exporter to twitter bootstrap html)
  (setq org-enable-bootstrap-support t)
  :config
  ;; make windmove work with org mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; org-capture - needs to be in :config because it assumes a variable is already defined: `org-directory'
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;;(add-hook 'org-mode-hook
  ;;          (lambda ()
  ;;        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))

  (defun org-export-turn-on-syntax-highlight()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'"
    (interactive)
    (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")))

  ;; compile with pdf-latex
  ;; (setq org-latex-pdf-process
  ;;     '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

  ;; compile with xelatex to use the Arial font
  (setq org-latex-pdf-process
    '("xelatex -interaction nonstopmode %f"
       "xelatex -interaction nonstopmode %f")) ;; for multiple passes

  (setq org-emphasis-alist '(("*" bold)
                              ("/" italic)
                              ("_" underline)
                              ("=" org-verbatim verbatim)
                              ("~" org-code verbatim)))


  (require 'org-habit)
  '(org-emphasis-alist
     (quote
       (
         ("!" org-habit-overdue-face)
         ("%" org-habit-alert-face)
         ("*" bold)
         ("/" italic)
         ("_" underline)
         ("=" org-verbatim verbatim)
         ("~" org-code verbatim)
         ("+" (:strike-through t))
         )))
  )


;; ** org-sidebar

;; In the tree buffer, the keymap org-sidebar-tree-map is used, which is based on org-mode-map (so you can use Org keybindings to manipulate nodes), and has these additional bindings by default:

;; - <S-tab>: Cycle global node visibility.
;; - <tab>: Toggle visibility of child nodes.
;; - <mouse-3>: Toggle visibility of child nodes.
;; - <mouse-2>: Jump to heading using default jump function; or, if heading stars are clicked, toggle visibility of child nodes.
;; - <return>: Jump to heading using default jump function (adding universal prefix arguments to display more subtree content, corresponding with the click-and-drag mouse events below).

;; Dragging-and-releasing with mouse buttons (as opposed to clicking and releasing at a single position) shows additional subtree and entry content:

;; - <drag-mouse-1>: Jump to heading using default jump function, and also show all descendant headings.
;; - <drag-mouse-2>: Jump to heading using default jump function, and also show all descendant headings and their entry text.


(use-package org-sidebar
  :ensure t
  :requires org-ql
  :defer t
  :after org
  :bind
  ("C-c o <f8>" . org-sidebar-tree-toggle)
  )

;; ** org-ql (required by org-sidebar)

(use-package org-ql
  :ensure t
  :defer t
  :after org
  )

;; ** ox-extra (org-plus-contrib)
;; add suport for the ignore tag (ignores a headline without ignoring its content)

(use-package ox-extra
  :ensure nil
  :defer t
  :config
  (ox-extras-activate '(ignore-headlines))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  )

;; ** ox-latex

(use-package ox-latex
  :ensure nil
  :defer t
  :after org
  :config
  ;; Source https://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
  ;; but adapted to use latexmk 4.20 or higher.
  ;; (defun my-auto-tex-cmd ()
  ;;   "When exporting from .org with latex, automatically run latex,
  ;;      pdflatex, or xelatex as appropriate, using latexmk."
  ;;   (let ((texcmd)))
  ;;   ;; default command: oldstyle latex via dvi
  ;;   (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;;   ;; pdflatex -> .pdf
  ;;   (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
  ;;       (setq texcmd "latexmk -pdf -quiet %f"))
  ;;   ;; xelatex -> .pdf
  ;;   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
  ;;       (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
  ;;   ;; LaTeX compilation command
  ;;   (setq org-latex-to-pdf-process (list texcmd)))
  ;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
  )

;; ** ox-pandoc

(use-package ox-pandoc
  :after (org ox)
  :defer t
  :config
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "luatex")))
  ;; special extensions for markdown_github output
  (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
  )

;; ** org-bullets

;; UTF8 pretty bullets in org mode


(use-package org-bullets
  :ensure t
  :after org
  :defer t
  :config
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "●" "►" "•"))
  )


;; ** ReveaJS org-reveal:

;; : This delay makes the options to export to RevealJS appear on the exporter menu (C-c C-e)


(use-package ox-reveal
  :ensure t
  :defer t
  :after ox
  :config
  ;;(setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
  )


;; ** ox-markdown

(use-package ox-md
  :ensure nil
  :defer t
  :after org
  )


;; ** ox-gfm (github-flavored markdown)

(use-package ox-gfm
  :ensure t
  :defer t
  :after org
  )


;; **************************************************

;; * SHELL INTEGRATION

;; ** shell-pop

(use-package shell-pop
  :ensure t
  :defer t
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
  )


;; ** System Shell
;; *** Make system shell open in a split-window buffer at the bottom of the screen


(defun /shell/new-window ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                   default-directory))
          (height (/ (window-total-height) 3))
          (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (shell "new")
    (rename-buffer (concat "*shell: " name "*"))
    (insert (concat "ls"))
    )
  )

                                        ; Pull system shell in a new bottom window
(define-key evil-normal-state-map (kbd "\"") #'/shell/new-window)
(define-key evil-visual-state-map (kbd "\"") #'/shell/new-window)
(define-key evil-motion-state-map (kbd "\"") #'/shell/new-window)


;; *** Make eshell open in a split-window buffer at the bottom of the screen

(defun /eshell/new-window ()
  "Opens up a new eshell in the directory associated with the current buffer's file.  The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                   (file-name-directory (buffer-file-name))
                   default-directory))
          (height (/ (window-total-height) 3))
          (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

                                        ; Pull eshell in a new bottom window
(define-key evil-normal-state-map (kbd "!") #'/eshell/new-window)
(define-key evil-visual-state-map (kbd "!") #'/eshell/new-window)
(define-key evil-motion-state-map (kbd "!") #'/eshell/new-window)


;; **************************************************

;; * PROJECT MANAGEMENT

;; ** Projectile

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (:map projectile-mode-map
    ("s-p" . projectile-command-map)
    ("C-c p" . projectile-command-map)
    ("M-S-O p" . counsel-projectile-switch-project)
    )
  :custom
  (projectile-completion-system 'ivy)
  :init
  (setq projectile-mode-line-prefix "Project -> ")
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  :config
  (projectile-mode +1)
  (setq projectile-globally-ignored-files
    (append '("~"
               ".swp"
               ".pyc")
      projectile-globally-ignored-files))
  )


;; **************************************************

;; * Ivy

(use-package ivy
  :ensure t
  :demand t
  :diminish ivy-mode
  :preface
  (defun ivy-format-function-pretty (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
      (lambda (str)
        (concat
          (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
          (ivy--add-face (concat str "\n") 'ivy-current-match)))

      (lambda (str)
        (concat "  " str "\n"))
      cands
      "")
    )
  :hook
  (after-init . ivy-mode)
  :custom
  (ivy-re-builders-alist
    '((t . ivy--regex-plus)))
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
  ;; display an arrow on the selected item in the list
  ;; (setf (cdr (assoc t ivy-format-functions-alist)) #'ivy-format-function-arrow)
  ;; (setq ivy-format-function #'ivy-format-function-line)
  ;; NOTE: this variable do not work if defined in :custom
  ;; (setf (cdr (assoc t ivy-format-functions-alist)) #'ivy-format-function-arrow)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-pretty)

  (ivy-set-actions  t
    '(("I" insert "insert")))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  )

;; ** counsel


(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  :defines
  (projectile-completion-system magit-completing-read-function)
  :hook
  (ivy-mode . counsel-mode)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-on-del-error-function nil)
  (counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  ;; Add smart-casing (-S) to default command arguments:
  (counsel-rg-base-command "rg -S --no-heading --line-number --color never %s .")
  (counsel-ag-base-command "ag -S --nocolor --nogroup %s")
  (counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")
  (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
  (counsel-find-file-at-point t)
  (counsel-yank-pop-height 15)
  (swiper-action-recenter t)
  ;; check out this better-jumper mode to see what it does
  ;; (counsel-grep-post-action . better-jumper-set-jump)
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  ([remap find-file] . counsel-find-file)
  ([find-file] . counsel-find-file)
  ([remap switch-to-buffer] . ivy-switch-buffer)
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
  ("C-c g g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("M-s c" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  ;; ladicle keys
  ("M-s r" . ivy-resume)
  ("C-c v p" . ivy-push-view)
  ("C-c v o" . ivy-pop-view)
  ("C-c v ." . ivy-switch-view)
  ("M-s f" . counsel-fzf)
  ("M-s r" . counsel-recentf)
  ("M-y" . counsel-yank-pop)
  (:map ivy-minibuffer-map
    ("C-w" . ivy-backward-kill-word)
    ("C-k" . ivy-kill-line)
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)
    ("C-h" . ivy-backward-delete-char))
  (:map minibuffer-local-map
    ("C-r" . counsel-minibuffer-history))
  :config
  (setq counsel-yank-pop-separator
    (propertize "\n────────────────────────────────────────────────────────\n"
      'face `(:foreground "#6272a4")))


  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  )

;; ** wgrep


(use-package wgrep
  :ensure t
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  )

;; ** Enhance M-x

;; Enhancement for `execute-extended-command'. Auto detects and uses ivy or ido, if installed
(use-package amx
  :ensure t
  :hook
  (after-init . amx-mode)
  )

;; ** Ivy integration for Projectile

(use-package counsel-projectile
  :ensure t
  :after ivy counsel projectile
  :config (counsel-projectile-mode 1)
  )


;; ** ivy-posframe

;; Requires: Emacs >= 26


(use-package ivy-posframe
  :ensure t
  :demand t
  :after ivy
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe ((t (:background "#333244"))))
  (ivy-posframe-border ((t (:background "#abff00"))))
  (ivy-posframe-cursor ((t (:background "#00ff00"))))
  :init
  (setq ivy-posframe-parameters '((internal-border-width . 2)))
  (setq ivy-posframe-width 130)
  ;; define the position of the posframe per function
  (setq ivy-posframe-display-functions-alist
    '((swiper          . ivy-posframe-display-at-window-center)
       (complete-symbol . ivy-posframe-display-at-point)
       ;;(counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
       (counsel-M-x     . ivy-posframe-display-at-frame-center)
       (t               . ivy-posframe-display-at-frame-center)))
  ;; custom define height of post frame per function
  (setq ivy-posframe-height-alist '((swiper . 10)
                                     (find-file . 20)
                                     (counsel-ag . 15)
                                     (counsel-projectile-ag . 30)
                                     (t      . 25)))
  :config
  (ivy-posframe-mode)
  )


;; ** ivy-rich

(use-package ivy-rich
  :ensure t
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

  ;; customizing functions
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
       )
    )

  :config
  ;; ivy-rich-mode needs to be called after `ivy-rich--display-transformers-list' is changed
  (ivy-rich-mode 1)

  )

;; ** ivy-prescient
(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode)
  )

;; all the icons for ivy

(use-package all-the-icons-ivy
  :ensure t
  :hook
  (after-init . all-the-icons-ivy-setup)
  )

;; **************************************************

;; * Hydra


(use-package hydra
  :ensure t
  :defer t
  )


;; ** major mode hydra

(use-package major-mode-hydra
  :ensure t)


;; **  hydra-posframe


(use-package hydra-posframe
  :load-path "packages/hydra-posframe"
  :custom
  (hydra-posframe-parameters
    '((left-fringe . 5)
       (right-fringe . 5)))
  :custom-face
  (hydra-posframe-border-face ((t (:background "#6272a4"))))
  :hook
  (after-init . hydra-posframe-mode)
  )

;; * occur mode

(use-package occur-mode
  :ensure nil
  :defer t
  :config
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)
  )


;; **************************************************

;; * FlyCheck linter

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :hook
  (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled newline))
  (flycheck-idle-change-delay 1.5)
  (flycheck-display-errors-delay 1)
  (flycheck-indication-mode 'left-fringe)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; (setq flycheck-checkers '(javascript-eslint typescript-tslint))
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-mode 'html-tidy 'sgml-mode)
  )


;; ** flycheck posframe

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :custom-face
  (flycheck-posframe-face ((nil (:background "#20fabe" :foreground "#FFCC0E"))))
  (flycheck-posframe-info-face ((nil (:inherit 'info))))
  (flycheck-posframe-warning-face ((nil (:background "#fcfa23" :foreground "#000000"))))
  (flycheck-posframe-warning-face ((nil (:inherit 'warning))))
  (flycheck-posframe-error-face ((nil (:inherit 'error))))
  (flycheck-posframe-background-face ((nil (:background "#FFFFFF" :foreground "#000000"))))
  (flycheck-posframe-border-face ((nil (:background "#af3ec8"))))
  :custom
  (flycheck-posframe-position 'point-bottom-left-corner)
  (flycheck-posframe-prefix "\u27a4 ") ;; default: ➤
  (flycheck-posframe-warning-prefix "\u26a0 ")
  (flycheck-posframe-info-prefix "\uf6c8 ")
  (flycheck-posframe-error-prefix "\u274c ")
  (flycheck-posframe-border-width 2)
  :config
  ;; Calling (flycheck-posframe-configure-pretty-defaults) will configure flycheck-posframe to show warnings and errors with nicer faces (inheriting from warning and error respectively), and set the prefix for each to nicer unicode characters.
  (flycheck-posframe-configure-pretty-defaults)
  )


;; **************************************************

;; * VERSION CONTROL

;; ** Magit

(use-package magit
  :ensure t
  :bind
  ("<tab>" . magit-section-toggle)
  ("M-g s" . magit-status)
  ("M-g f" . magit-find-file)
  ("M-g l" . magit-log)
  ("M-g b" . magit-blame)
  ("C-x g" . magit-status)
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

;; ** evil-magit

(use-package evil-magit
  :ensure t
  :init
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  :config
  (evil-magit-init)
  (evil-define-key evil-magit-state magit-mode-map "<tab>" 'magit-section-toggle)
  (evil-define-key evil-magit-state magit-mode-map "l" 'magit-log-popup)
  (evil-define-key evil-magit-state magit-mode-map "j" 'evil-next-visual-line)
  (evil-define-key evil-magit-state magit-mode-map "k" 'evil-previous-visual-line)
                                        ;(evil-define-key evil-magit-state magit-diff-map "k" 'evil-previous-visual-line)
  (evil-define-key evil-magit-state magit-staged-section-map "K" 'magit-discard)
  (evil-define-key evil-magit-state magit-unstaged-section-map "K" 'magit-discard)
  (evil-define-key evil-magit-state magit-unstaged-section-map "K" 'magit-discard)
  (evil-define-key evil-magit-state magit-branch-section-map "K" 'magit-branch-delete)
  (evil-define-key evil-magit-state magit-remote-section-map "K" 'magit-remote-remove)
  (evil-define-key evil-magit-state magit-stash-section-map "K" 'magit-stash-drop)
  (evil-define-key evil-magit-state magit-stashes-section-map "K" 'magit-stash-clear)
  )

;; ** diffview
;; View diffs side by side

(use-package diffview
  :ensure t
  :defer t
  )

;; ** magit-todo


(use-package magit-todos
  :ensure t
  :after magit hl-todo
  :bind
  ("M-g t" . magit-todos-list)
  :config
  (magit-todos-mode)
  )

;; ** vc-msg

(use-package vc-msg
  :ensure t
  :defer t
  :bind
  ("C-c g p" . vc-msg-show)
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close))
  )

;; Major modes for git related files

;; Mode for .gitignore files.
(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)


;; ** git-time-machine

;; Navigation through the history of files

(use-package git-timemachine
  :ensure t
  :bind
  ("C-c g t" . git-timemachine-toggle)
  )

(use-package forge
  :ensure t
  :after magit
  :config
  (setq forge-topic-list-limit '(30 . 5)
    forge-pull-notifications t)
  )

;; **************************************************

;; * elisp-format

(use-package elisp-format
  :ensure t
  :defer t
  )


;; **************************************************

;; * Autocomplete

;; ** Company

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun yas-expand-or-company-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.
If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
      (let ((old-point (point))
             (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                  (eq old-tick (buffer-chars-modified-tick)))
            (company-complete-common))))
      (company-complete-common)))
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
    ([tab] . yas-expand-or-company-complete)
    ("TAB" . yas-expand-or-company-complete))
  (:map evil-insert-state-map
    ;; ("<tab>" . company-indent-or-complete-common)
    ("C-SPC" . company-indent-or-complete-common))
  (:map company-active-map
    ("M-n" . nil)
    ("M-p" . nil)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("S-<backtab>" . company-select-previous)
    ("<backtab>" . company-select-previous)
    ("C-d" . company-show-doc-buffer))
  (:map company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next))
  :custom-face
  (company-preview-common ((t (:foreground unspecified :background "#111111"))))
  (company-scrollbar-bg ((t (:background "#111111"))))
  (company-scrollbar-fg ((t (:background "#555555"))))
  (company-tooltip ((t (:inherit default :background "#202029"))))
  (company-tooltip-common ((t (:inherit font-lock-constant-face))))
  (company-tooltip-selection ((t (:inherit company-tooltip-common :background "#2a2a2a" ))))
  :custom
  (company-minimum-prefix-length 1)                      ; start completing after 1st char typed
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)                             ; bigger popup window
  (company-begin-commands '(self-insert-command))        ; start autocompletion only after typing
  (company-idle-delay 0.1)                               ; decrease delay before autocompletion popup shows
  (company-echo-delay 0)                                 ; remove annoying blinking
  (company-selection-wrap-around t)                      ; continue from top when reaching bottom
  (company-auto-complete 'company-explicit-action-p)
  (company-require-match nil)
  (company-complete-number t)                            ;; Allow (lengthy) numbers to be eligible for completion.
  (company-show-numbers t)                               ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-transformers '(company-sort-by-occurrence))   ; weight by frequency
  ;; company-dabbrev
  (company-dabbrev-downcase nil)                         ;; Do not downcase completions by default.
  (company-dabbrev-ignore-case t)                        ;; Even if I write something with the ‘wrong’ case, provide the ‘correct’ casing.
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  :config
  (global-company-mode)
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; show tooltip even for single candidates
  (setq company-frontends '(company-pseudo-tooltip-frontend
                             company-echo-metadata-frontend))
  )

;; *** company prescient

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode)
  )

;; *** company quickHelp

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  ;; :defer t
  :hook
  (global-company-mode . company-quickhelp-mode)
  :bind
  (:map company-active-map
    ("M-h" . company-quickhelp-manual-begin)
    )
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode)
  )


;; *** Company postframe

;; : PS: this looks exactly the same as the usual company popup, except it doesn't disturb other overlays (like line numbers) in the buffer.

(use-package company-posframe
  :ensure t
  :diminish company-posframe-mode
  :after company
  :hook
  (company-mode . company-posframe-mode)
  (global-company-mode . company-posframe-mode)
  )


;; *** Company Box (icons in suggestions)

;; CompanyBoxPac
(use-package company-box
  :ensure t
  :diminish
  :hook ((global-company-mode company-mode) . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.2)
  )


;; ** LSP

;; *** LSP (language server protocol implementation for emacs)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (prog-mode . lsp)
  (text-mode . lsp)
  :custom
  ;; general
  ;; (lsp-auto-configure t) ;; auto-configure `lsp-ui' and `company-lsp'
  (lsp-prefer-flymake nil) ;; t: flymake | nil: lsp-ui | :none -> none of them (flycheck)
  :config
  ;; angular language server
  (setq lsp-clients-angular-language-server-command
    '("node"
       "/home/gustavo/.nvm/versions/node/v10.16.3/lib/node_modules/@angular/language-server"
       "--ngProbeLocations"
       "/home/gustavo/.nvm/versions/node/v10.16.3/lib/node_modules"
       "--tsProbeLocations"
       "/home/gustavo/.nvm/versions/node/v10.16.3/lib/node_modules"
       "--stdio"))
  )

;; *** lsp ui

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom-face
  (lsp-ui-doc-background ((nil (:background "#f9f2d9"))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  (lsp-ui-sideline-global ((nil (:inherit 'shadow :background "#f9f2d9"))))
  :bind
  (:map lsp-mode-map
    ("C-c l p f r" . lsp-ui-peek-find-references)
    ("C-c l p f d" . lsp-ui-peek-find-definitions)
    ("C-c l p f i" . lsp-ui-peek-find-implementation)
    ("C-c l g d" . lsp-goto-type-definition)
    ("C-c l f d" . lsp-find-definition)
    ("C-c l g i" . lsp-goto-implementation)
    ("C-c l f i" . lsp-find-implementation)
    ("C-c l m"   . lsp-ui-imenu)
    ("C-c l s"   . lsp-ui-sideline-mode)
    ("C-c l d"   . tau/toggle-lsp-ui-doc))
  ;; remap native find-definitions and references to use lsp-ui
  (:map lsp-ui-mode-map
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references)
    ("C-c u" . lsp-ui-imenu))
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-max-width 100)
  (lsp-ui-doc-max-height 30)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable nil) ;; disable to leave `tslint' as checker for ts files
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t) ;; show diagnostics (flyckeck) messages in sideline
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 40)
  :config
  ;; lsp-ui appearance
  (add-hook 'lsp-ui-doc-frame-hook
    (lambda (frame _w)
      (set-face-attribute 'default frame :font "Hack 11")))
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))
  )

;; *** company-lsp

;; Company-lsp is auto inserted into company backends

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp
  :custom
  ;; (company-lsp-enable-snippet t)
  ;; (company-lsp-async t)
  (company-lsp-cache-candidates 'auto)
  ;; (company-lsp-enable-recompletion t)
  )

;; **************************************************


;; *** lsp-ivy

(use-package lsp-ivy
  :ensure t
  :defer t
  :bind
  ("C-c l i s" . lsp-ivy-workspace-symbol)
  )


;; **************************************************



;; ** Yasnippets

(use-package yasnippet
  :ensure t
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
  ;; set snippets directory
  ;;  (setq yas-snippet-dirs '(yasnippet-snippets-dir))
  ;; add angular snippets folder
  (with-eval-after-load 'yasnippets-snippets
    (setq yas-snippet-dirs (append yas-snippet-dirs
                             '("~/dotfiles/emacs.d/snippets/angular/"))))
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (yas-reload-all) ;; tell yasnippet about updates to yas-snippet-dirs
  ;; disabled global mode in favor or hooks in prog and text modes only
  ;; (yas-global-mode 1)
  )

;; Colection of snippets
(use-package yasnippet-snippets :ensure t)

;; **************************************************

;; * File Explorers

;; ** ivy-explorer

(use-package ivy-explorer
  :ensure t
  :diminish
  :after ivy
  :hook
  (after-init . ivy-explorer-mode)
  )

;; ** Dired

;; Make dired look like k

(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-dwin-target t)
  (add-hook 'dired-initial-position-hook #'dired-k)
  )

;; ** all the icons dired

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )

;; **************************************************
;; ** Treemacs itself

(use-package treemacs
  :ensure t
  :defer t
  :hook
  (after-init . treemacs)
  :bind
  (:map global-map
    ("<f8>"       . treemacs)
    ("M-0"       . treemacs-select-window)
    ("C-x t 1"   . treemacs-delete-other-windows)
    ("C-x t t"   . treemacs)
    ("C-x t B"   . treemacs-bookmark)
    ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag))
  :custom
  ;; general variables
  (treemacs-no-png-images nil)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-eldoc-display t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config

  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0))
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (linum-mode -1)
                                  (fringe-mode 0)
                                  ;; (setq buffer-face-mode-face `(:background "#211C1C"))
                                  (buffer-face-mode 1)))


  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
           (not (null treemacs-python-executable)))
    (`(t . t)
      (treemacs-git-mode 'deferred))
    (`(t . _)
      (treemacs-git-mode 'simple)))

  ;; my vscode icon theme, using vscode-icons
  ;; i just placed the vscode-icons folder inside the treemacs icons vfolder and changed paths
  (defcustom tau-themes-treemacs-theme "vscode"
    "Default treemacs theme."
    :type '(radio (const :doc "A minimalistic atom-inspired icon theme" "doom-atom")
             (const :doc "A colorful icon theme leveraging all-the-icons" "doom-colors"))
    :group 'tau-themes-treemacs)

  ;; warn if all-the-icons isnt sinstalled
  (with-eval-after-load 'treemacs
    (unless (require 'all-the-icons nil t)
      (error "all-the-icons isn't installed"))

    (let ((face-spec '(:inherit font-lock-doc-face :slant normal)))  ;; taken from doom-treemacs -theme to use all theicons in some parts
      (treemacs-create-theme "vscode"
        :icon-directory (f-join treemacs-dir "icons/default")
        :config
        (progn
          ;; directory and other icons
          ;; (treemacs-create-icon :file "root.png"        :extensions (root)       :fallback "")
          (treemacs-create-icon
            :icon (format " %s\t" (all-the-icons-octicon "repo" :height 1.2 :v-adjust -0.1 :face face-spec))
            :extensions (root))
          (treemacs-create-icon :file "vscode/default_folder.png"  :extensions (dir-closed) :fallback (propertize "+ " 'face 'treemacs-term-node-face))
          (treemacs-create-icon :file "vscode/default_folder_opened.png"    :extensions (dir-open)   :fallback (propertize "- " 'face 'treemacs-term-node-face))
          (treemacs-create-icon :file "tags-leaf.png"   :extensions (tag-leaf)   :fallback (propertize "• " 'face 'font-lock-constant-face))
          (treemacs-create-icon :file "tags-open.png"   :extensions (tag-open)   :fallback (propertize "▸ " 'face 'font-lock-string-face))
          (treemacs-create-icon :file "tags-closed.png" :extensions (tag-closed) :fallback (propertize "▾ " 'face 'font-lock-string-face))
          (treemacs-create-icon :file "error.png"       :extensions (error)      :fallback (propertize "• " 'face 'font-lock-string-face))
          (treemacs-create-icon :file "warning.png"     :extensions (warning)    :fallback (propertize "• " 'face 'font-lock-string-face))
          (treemacs-create-icon :file "info.png"        :extensions (info)       :fallback (propertize "• " 'face 'font-lock-string-face))

          ;; common file types icons
          (treemacs-create-icon :file "vscode/default_file.png"         :extensions (fallback))
          (treemacs-create-icon :file "vscode/image.png"       :extensions ("jpg" "jpeg" "bmp" "svg" "png" "xpm" "gif"))
          (treemacs-create-icon :file "vscode/video.png"       :extensions ("webm" "mp4" "avi" "mkv" "flv" "mov" "wmv" "mpg" "mpeg" "mpv"))
          (treemacs-create-icon :file "vscode/pdf.png"         :extensions ("pdf"))
          (treemacs-create-icon :file "vscode/emacs.png"       :extensions ("el" "elc"))
          (treemacs-create-icon :file "ledger.png"      :extensions ("ledger"))
          (treemacs-create-icon :file "vscode/config.png"        :extensions ("properties" "conf" "config" "cfg" "ini" "xdefaults" "xresources" "terminalrc" "ledgerrc"))
          (treemacs-create-icon :file "vscode/shell.png"       :extensions ("sh" "zsh" "fish"))
          (treemacs-create-icon :file "asciidoc.png"    :extensions ("adoc" "asciidoc"))
          ;; git
          (treemacs-create-icon :file "vscode/git.png"         :extensions ("git" "gitignore" "gitconfig" "gitmodules" "gitattributes"))
          ;; dev lib
          (treemacs-create-icon :file "vscode/editorconfig.png"         :extensions ("editorconfig"))
          ;; frontend universe
          (treemacs-create-icon :file "vscode/json.png"        :extensions ("json"))
          (treemacs-create-icon :file "vscode/html.png"        :extensions ("html" "htm"))
          (treemacs-create-icon :file "vscode/css.png"         :extensions ("css"))
          (treemacs-create-icon :file "vscode/scss.png"         :extensions ("scss"))
          (treemacs-create-icon :file "vscode/js_official.png"          :extensions ("js" "jsx"))
          (treemacs-create-icon :file "vscode/typescript.png"          :extensions ("ts" "tsx"))
          (treemacs-create-icon :file "vscode/typescriptdef.png"          :extensions ("spec"))
          (treemacs-create-icon :file "vscode/tslint.png"          :extensions ("tslint"))
          (treemacs-create-icon :file "vscode/tsconfig.png"          :extensions ("tsconfig"))
          (treemacs-create-icon :file "vscode/vue.png"         :extensions ("vue"))
          (treemacs-create-icon :file "vscode/elm.png"         :extensions ("elm"))
          ;; markupgs
          (treemacs-create-icon :file "vscode/org.png"     :extensions ("org"))
          (treemacs-create-icon :file "vscode/markdown.png"    :extensions ("md"))
          (treemacs-create-icon :file "vscode/tex.png"         :extensions ("tex"))
          (treemacs-create-icon :file "vscode/yaml.png"        :extensions ("yml" "yaml"))
          (treemacs-create-icon :file "vscode/toml.png"        :extensions ("toml"))
          (treemacs-create-icon :file "vscode/dartlang.png"        :extensions ("dart"))
          (treemacs-create-icon :file "vscode/julia.png"       :extensions ("jl"))
          ;; erlang / elixir
          (treemacs-create-icon :file "vscode/erlang2.png"      :extensions ("erl" "hrl"))
          (treemacs-create-icon :file "vscode/elixir.png"         :extensions ("ex"))
          (treemacs-create-icon :file "elx-light.png"   :extensions ("exs" "eex"))
          ;; ruby
          (treemacs-create-icon :file "ruby.png"   :extensions ("rb"))
          (treemacs-create-icon :file "erb.png"   :extensions ("erb"))
          ;; backend languages file types
          (treemacs-create-icon :file "vscode/rust.png"        :extensions ("rs"))
          (treemacs-create-icon :file "vscode/clojure.png"     :extensions ("clj" "cljs" "cljc"))
          (treemacs-create-icon :file "vscode/java.png"        :extensions ("java"))
          (treemacs-create-icon :file "vscode/kotlin.png"      :extensions ("kt"))
          (treemacs-create-icon :file "vscode/scala.png"       :extensions ("scala"))
          (treemacs-create-icon :file "sbt.png"         :extensions ("sbt"))
          (treemacs-create-icon :file "vscode/go.png"          :extensions ("go"))
          (treemacs-create-icon :file "vscode/php.png"         :extensions ("php"))
          (treemacs-create-icon :file "vscode/c.png"           :extensions ("c" "h"))
          (treemacs-create-icon :file "vscode/cpp.png"         :extensions ("cpp" "cxx" "hpp" "tpp" "cc" "hh"))
          ;; lisp ecosystem
          (treemacs-create-icon :file "racket.png"      :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
          ;; haskell
          (treemacs-create-icon :file "vscode/haskell.png"     :extensions ("hs" "lhs"))
          (treemacs-create-icon :file "cabal.png"       :extensions ("cabal"))
          ;; python
          (treemacs-create-icon :file "python.png"      :extensions ("py" "pyc"))
          (treemacs-create-icon :file "hy.png"          :extensions ("hy"))
          (treemacs-create-icon :file "ocaml.png"       :extensions ("ml" "mli"))
          (treemacs-create-icon :file "puppet.png"      :extensions ("pp"))
          ;; devops tools
          (treemacs-create-icon :file "vscode/docker.png"      :extensions ("dockerfile"))
          (treemacs-create-icon :file "vagrant.png"     :extensions ("vagrantfile"))
          (treemacs-create-icon :file "jinja2.png"      :extensions ("j2" "jinja2"))
          (treemacs-create-icon :file "purescript.png"  :extensions ("purs"))
          (treemacs-create-icon :file "nix.png"         :extensions ("nix"))
          (treemacs-create-icon :file "scons.png"       :extensions ("sconstruct" "sconstript"))
          (treemacs-create-icon :file "vscode/make.png"    :extensions ("makefile"))
          (treemacs-create-icon :file "vscode/license.png" :extensions ("license"))
          (treemacs-create-icon :file "vscode/zip.png"     :extensions ("zip" "7z" "tar" "gz" "rar"))
          (treemacs-create-icon :file "vscode/elm.png"     :extensions ("elm"))
          (treemacs-create-icon :file "vscode/xml.png"     :extensions ("xml" "xsl"))
          (treemacs-create-icon :file "vscode/binary.png"  :extensions ("exe" "dll" "obj" "so" "o"))
          (treemacs-create-icon :file "vscode/ruby.png"    :extensions ("rb"))
          (treemacs-create-icon :file "vscode/scss.png"    :extensions ("scss"))
          (treemacs-create-icon :file "vscode/lua.png"     :extensions ("lua"))
          (treemacs-create-icon :file "vscode/log.png"     :extensions ("log"))
          (treemacs-create-icon :file "vscode/lisp.png"    :extensions ("lisp"))
          (treemacs-create-icon :file "vscode/sql.png"     :extensions ("sql"))
          (treemacs-create-icon :file "vscode/nim.png"     :extensions ("nim"))
          (treemacs-create-icon :file "vscode/perl.png"    :extensions ("pl" "pm" "perl"))
          (treemacs-create-icon :file "vscode/vim.png"     :extensions ("vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"))
          (treemacs-create-icon :file "vscode/deps.png"    :extensions ("cask"))
          (treemacs-create-icon :file "vscode/r.png"       :extensions ("r"))
          (treemacs-create-icon :file "vscode/reason.png"  :extensions ("re" "rei")))))

    ;; finally apply the custom theme
    (treemacs-load-theme tau-themes-treemacs-theme))

  ;;apply treemacs icon theme
  (treemacs-load-theme "vscode")
  (treemacs-resize-icons 18) ;; usefull on high dpi monitors.  default icon size is 22

  )


;; *** Treemacs Evil

(use-package treemacs-evil
  :after treemacs evil
  :ensure t
  )


;; *** Treemacs Projectile

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t
  )

;; *** Treemacs Dired

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode)
  )


;; *** Treemacs Magit

(use-package treemacs-magit
  :after treemacs magit
  :ensure t
  )


;; **************************************************

;; ** ranger

(use-package ranger
  :ensure t
  :defer t
  :bind
  ("C-x C-j" . ranger)
  :config
  (setq ranger-show-hidden t) ;; show hidden files
  )




;; **************************************************

;; * Appearance / UI

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; scroll bars from frames
(scroll-bar-mode -1)

;; Remove menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; set background and foreground color
(set-background-color "#111111")
(set-foreground-color "#dddddd")

;; Applying my theme
(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")
                                        ; theme options:
                                        ; atom-one-dark (doenst work well with emacsclient, ugly blue bg)
                                        ; dracula
                                        ; darktooth
                                        ; gruvbox-dark-hard
                                        ; gruvbox-dark-light
                                        ; gruvbox-dark-medium
                                        ; base16-default-dark-theme -- this one is good

;;  Load the theme
(load-theme my-theme t)


;; ** vi tilde fringe

;; Displays tildes in the fringe on empty lines a la Vi.

(use-package vi-tilde-fringe
  :ensure t
  :diminish
  :config
  (global-vi-tilde-fringe-mode)
  )

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)
;; Set the whitespace highlight color
(set-face-background 'trailing-whitespace "#f44545")

;; **************************************************

;; ** Fonts and Icons

;; set default font

;; Find the first font in the list and use it
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

;; define list of fonts to be used in the above function
;; the first one found will be used
(set-face-attribute 'default nil :font (font-candidate '"Hack-10:weight=normal"
                                         "Consolas-10:weight=normal"
                                         "Droid Sans Mono-10:weight=normal"
                                         "DejaVu Sans Mono-10:weight=normal"
                                         "Ubuntu Mono-12:weight=normal"))

;; ** Increase, decrease and adjust font size
(global-set-key (kbd "C-S-+") #'text-scale-increase)
(global-set-key (kbd "C-S-_") #'text-scale-decrease)
(global-set-key (kbd "C-S-)") #'text-scale-adjust)

(setq echo-keystrokes 0.02)

;; ** All The Icons - Icon package

(use-package all-the-icons
  :if window-system
  :ensure t
  :commands
  (all-the-icons-octicon
    all-the-icons-faicon
    all-the-icons-fileicon
    all-the-icons-wicon
    all-the-icons-material
    all-the-icons-alltheicon)
  )


;; ** vscode-icons

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file)
  )

;; ** Add some information about buffer boundaries in the left fringe.

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)


(setq display-time-24hr-format t)
(display-time-mode +1)

;; **************************************************

;; ** STARTUP STUFF

;; appearantly the `inhibit-splash-screen' was deprecaded. uses `inhibit-startup-screen' now
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
;; Don't show *Buffer list* when opening multiple files at the same time.
(setq initial-buffer-choice nil)
;; Makes *scratch* empty.
(setq initial-scratch-message nil)
;; Set the initial major mode on startup
;;(setq initial-major-mode 'org-mode)
(setq inhibit-startup-buffer-menu t)
;; Make the buffer that opens on startup your init file ("~/.emacs" or "~/.emacs.d/init.el").
;;(setq initial-buffer-choice user-init-file)



;; ** Uniquify (unique files names in buffers)

(use-package uniquify
  :defer 1
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t)
  )



;; ** solaire-mode

;; solaire-mode is an aesthetic plugin that helps visually distinguish
;; file-visiting windows from other types of windows (like popups or sidebars)
;; by giving them a slightly different -- often brighter -- background.


(use-package solaire-mode
  :ensure t
  :hook
  (change-major-mode . turn-on-solaire-mode)
  (after-revert . turn-on-solaire-mode)
  (ediff-prepare-buffer . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  ;; if you use auto-revert-mode, this prevents solaire-mode from turning itself off every time Emacs reverts the file
  (after-revert . turn-on-solaire-mode)
  :config
  (solaire-global-mode)
  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;; NOTE: This is necessary for themes in the doom-themes package!
  ;; (solaire-mode-swap-bg)
  )

;; ** dimmer

;; Dim unused frames and windows
;; Focus current window and dim unused ones

(use-package dimmer
  :disabled
  :ensure t
  :config (dimmer-mode)
  )

;; **************************************************

;; * Frame and Windows

(dolist (frame (frame-list))
  (set-frame-parameter frame 'bottom-divider-width 1)
  (set-frame-parameter frame 'right-divider-width 1))


(push (cons 'bottom-divider-width 1) default-frame-alist)
(push (cons 'right-divider-width 1) default-frame-alist)


;; * Window and frame management and navigation

;; ** auto balance windows area


(global-set-key (kbd "C-M-+") 'balance-windows-area)


;; ** eyebrowse
(use-package eyebrowse
  :hook
  (after-init . eyebrowse-mode)
  :bind
  (:map eyebrowse-mode-map
    ("M-1" . eyebrowse-switch-to-window-config-1)
    ("M-2" . eyebrowse-switch-to-window-config-2)
    ("M-3" . eyebrowse-switch-to-window-config-3)
    ("M-4" . eyebrowse-switch-to-window-config-4)
    ("H-<right>" . eyebrowse-next-window-config)
    ("H-<left>" . eyebrowse-prev-window-config))
  :config
  ;;(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  ;;(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  ;;(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  ;;(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  ;;(define-key eyebrowse-mode-map (kbd "H-<right>") 'eyebrowse-next-window-config)
  ;;(define-key eyebrowse-mode-map (kbd "H-<left>") 'eyebrowse-prev-window-config)
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  )


;; ** ace-window

(use-package ace-window
  :ensure t
  :functions hydra-frame-window/body
  :bind
  ("M-o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :config
  ;; (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; set the window labels in the home row
  )

;; ** emacsrotate

;; This shares bindings with ace-window


(use-package rotate
  :ensure t
  :defer t
  :bind
  ("C-c r w" . rotate-window)
  ("C-c r l" . rotate-layout)
  ("M-S-O SPC" . rotate-layout)
  ("M-S-O m" . tau/toggle-window-maximize)
  ("C-M-o" . hydra-frame-window/body)
  :preface
  (defvar is-window-maximized nil)
  (defun tau/toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
        (balance-windows)
        (maximize-window))
      (setq is-window-maximized
        (not is-window-maximized))))
  (defun hydra-title(title) (propertize title 'face `(:inherit font-lock-warning-face :weight bold)))
  (defun command-name(title) (propertize title 'face `(:foreground "#f8f8f2")))
  (defun spacer() (propertize "." 'face `(:foreground "#282a36")))
  :config
  (with-eval-after-load 'hydra
    (defhydra hydra-frame-window (:color blue :hint nil)
      (format
        (format "%s" (propertize "                                                                       ╔════════╗
  ((%s))^^^^^^^^   ((%s))^^^^  ((%s))^^  ((%s))^^  ((%s))^^^^^^  ((%s))^   ║ Window ║
^^^^^^ ──────────────────────────────────────────────────────────────────────╨────────╜
      ^_k_^        %s_+_         _-_       %s     _,_ ← %s → _._^  %s
      ^^↑^^          ^↑^         ^↑^       %s
  _h_ ←   → _l_   ^^%s%s^^^^^    ^%s    ^^^%s^^^^     %s
      ^^↓^^          ^↓^         ^↓^       %s^^       %s
      ^_j_^        %s_=_         _/_       %s
^^^^^^ ┌──────────────────────────────────────────────────────────────────────────────┘
                         [_q_]: %s, [_<SPC>_]: %s" 'face `(:inherit font-lock-doc-face)))
        (hydra-title "Size")
        (hydra-title "Zoom")
        (hydra-title "Split")
        (hydra-title "Window")
        (hydra-title "Buffer")
        (hydra-title "Misc")
        (all-the-icons-material "zoom_in" :height .85 :face 'font-lock-doc-face)
        (command-name "_o_ther")
        (command-name "page")
        (command-name "_r_centf")
        (command-name "_s_wap")
        (all-the-icons-faicon "slideshare" :height .85 :face 'font-lock-doc-face)
        (command-name "_p_mode")
        (command-name "w_i_ndow")
        (command-name "_m_aximize")
        (command-name "_s_witch")
        (command-name "_d_elete")
        (command-name "_D_elete")
        (all-the-icons-material "zoom_out" :height .85 :face 'font-lock-doc-face)
        (command-name "del_O_thers")
        (command-name "quit")
        (command-name "rotate")
        )

      ("K" kill-current-buffer :exit t)
      ("D" kill-buffer-and-window :exit t)
      ("O" delete-other-windows  :exit t)
      ("F" toggle-frame-fullscreen)
      ("i" ace-window)
      ("s" ace-swap-window :exit t)
      ("d" ace-delete-window)
      ("m" tau/toggle-window-maximize :exit t)
      ("=" text-scale-decrease)
      ("+" text-scale-increase)
      ("-" split-window-vertically)
      ("/" split-window-horizontally)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("," previous-buffer)
      ("." next-buffer)
      ("o" other-window)
      ("p" presentation-mode)
      ("r" counsel-recentf :exit t)
      ("s" switch-to-buffer :exit t)
      ("D" kill-buffer-and-window)
      ("<SPC>" rotate-layout)
      ("q" nil)))
  )



;; ** windmove

(use-package windmove
  :ensure t
  :config
  ;; use shift + arrow keys to switch between visible buffers
  ;; (windmove-default-keybindings)
  (windmove-default-keybindings 'control)
  (global-set-key (kbd "C-S-H") 'windmove-left)
  (global-set-key (kbd "C-S-L") 'windmove-right)
  (global-set-key (kbd "C-S-K") 'windmove-up)
  (global-set-key (kbd "C-S-J") 'windmove-down)
  )


;; ** zoom

;; Zoom in selected window and resize othersAuto balance windows and frames


(use-package zoom
  :ensure t
  :bind
  ("C-M-z" . zoom)
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(treemacs dired-mode neotree dired-sidebar))
  (zoom-ignored-buffer-names '("zoom.el" "init.el"))
  (zoom-ignored-buffer-name-regexps '("^*calc"))
  :config
  (zoom-mode t)
  )

;; **************************************************

;; * File navigation

;; ** swiper

;; Swiper is an alternative to isearch that uses ivy to show an overview of all matches.

(use-package swiper
  :disabled
  :ensure t
  :bind
  (("C-s" . swiper-isearch)
    :map swiper-map
    ("M-q" . swiper-query-replace)
    ("C-l". swiper-recenter-top-bottom)
    ("C-'" . swiper-avy))
  :custom
  (counsel-grep-swiper-limit 20000)
  (counsel-rg-base-command
    "rg -i -M 120 --no-heading --line-number --color never %s .")
  (counsel-grep-base-command
    "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  )

;; ** avy

(use-package avy
  :ensure t
  :bind
  ("M-g j" . avy-goto-char-2)
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ;; replace native M-g c `goto-char' with `avy-goto-char'
  ([remap goto-char] . avy-goto-char)
  ;; replace native M-g g `goto-line' with `avy-goto-line'
  ;; ("M-g g" .  avy-goto-line) ;; disabled in favor of goto-line-preview
  ("M-g f" .  avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  (:map isearch-mode-map
    ("C-'" . avy-search))
  (:map evil-normal-state-map
    ("SPC" . avy-goto-char))
  (:map evil-visual-state-map
    ("SPC" . avy-goto-char))
  :config
  (global-set-key [remap goto-char] 'avy-goto-char)
  (setq avy-background nil) ;; default nil ;; gray background will be added during the selection.
  (setq avy-highlight-first t) ;; When non-nil highlight the first decision char with avy-lead-face-0. Do this even when the char is terminating.

  ;; nil: use only the selected window
  ;; t: use all windows on the selected frame
  ;; all-frames: use all windows on all frames
  (setq avy-all-windows nil)
  )

;; ** goto-line-preview

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
  )

;; * miniwindow and minibuffer

(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

;; ** minibuffer history
(savehist-mode 1)


;; **************************************************
;;
;;; ** MODELINE AND HEADERLINE

;; Modeline colors
(set-face-attribute 'mode-line nil :height font-size/mode-line)
(set-face-attribute 'mode-line nil
  :background color/modeline-active-bg
  :foreground color/modeline-active-fg
  :box '(:line-width 3 :color "#007af0") ;; modeline border
  :overline nil
  :underline nil)

;; for now the inactive modeline looks the same as the active one
(set-face-attribute 'mode-line-inactive nil
  :background color/modeline-inactive-bg
  :foreground color/modeline-inactive-fg
  :box '(:line-width 3 :color "#007ad3") ;; modeline border
  :overline nil
  :underline nil)

;; ** My personal modeline

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; ------------------------------
    "state: "
    evil-mode-line-tag
    ;; ------------------------------
    "| "
    "front-space: "
    mode-line-front-space
    ;; ------------------------------
    "| "
    "mule-info: "
    mode-line-mule-info
    ;; ------------------------------
    "| "
    "buffer status: "
    mode-line-modified
    "| "
    mode-line-buffer-identification
    "teste aqui"
    mode-line-position
    mode-line-modes
    ;; value of `mode-name'
    "%m: "
    ;; value of current buffer name
    "| "
    "buffer %b, "
    ;; value of current line number
    "| "
    "line %l "
    "| "
    ;; current user
    "user: "
    (getenv "USER")
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
              'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
    (propertize "%02l" 'face 'font-lock-type-face) ","
    (propertize "%02c" 'face 'font-lock-type-face)
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    )
  )

;; Diplay line number on the modeline
(line-number-mode t)
;; Diplay column number on the modeline
(column-number-mode t)
;; Diplay file size on the modeline
(size-indication-mode t)


;; Display the value of point in the mode line. It is displayed in brackets, adjacent to the line and/or column number if those are being displayed.

(use-package show-point-mode
  :ensure nil
  :load-path "packages/show-point-mode"
  :config
  (show-point-mode t)
  )


;; ** anzu

;; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which displays current match and total matches information in the mode-line in various search modes.

(use-package anzu
  :ensure t
  :bind
  (:map isearch-mode-map
    ([remap isearch-query-replace] . anzu-isearch-query-replace)
    ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :custom-face
  (anzu-mode-line ((nil (:foreground "yellow" :weight 'bold))))
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  )

;; ** evil-anzu

;; (use-package evil-anzu
;;   :ensure t
;;   :after evil
;;   )

;; ** hide mode line mode

(use-package hide-mode-line
  :ensure t
  :hook
  (completion-list-mode . hide-mode-line-mode)
  (neotree-mode . hide-mode-line-mode)
  (treemacs-mode . hide-mode-line-mode)
  )

;; ** mini-modeline

;; Display the modeline in the minibuffer

(use-package mini-modeline
  :ensure t
  :config
  ;;(setq mini-modeline-l-format) ;; Left part of mini-modeline, same format with mode-line-format.
  ;;(setq mini-modeline-r-format) ;; Right part of mini-modeline, same format with mode-line-format.
  (setq mini-modeline-color "#202020") ;; Background of mini-modeline. Will be set if mini-modeline-enhance-visual is t.
  (setq mini-modeline-enhance-visual t) ;; Enhance minibuffer and window's visibility. This will enable window-divider-mode since without the mode line, two continuous windows are nearly indistinguishable.
  (setq mini-modeline-echo-duration 4) ;; default 2 ; Duration to keep display echo. mini-modeline will display the message which has been echoed to echo area as part of mode line. Those echo will be automatically clear after this interval. Check out the gif to see it in action.
  (setq mini-modeline-update-interval 0.1) ;; default 0.1 ; The minimum interval to update mini-modeline. If you found mini-modeline is being updated to frequently, you can customize this variable.
  (setq mini-modeline-frame nil) ;; default nil ; Frame to display mini-modeline on. nil means current selected frame.
  (setq mini-modeline-truncate-p nil) ;; Truncates the mini-modeline to fit in one line.
  ;;(mini-modeline-mode t)
  )

;; ** minions

;; group all minor modes in a single menu in the modeline

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  )

;; ** parrot-mode

(use-package parrot
  :ensure t
  :config
  ;; To see the party parrot in the modeline, turn on parrot mode:
  (parrot-mode)
  (parrot-set-parrot-type 'default)
  ;; Rotate the parrot when clicking on it (this can also be used to execute any function when clicking the parrot, like 'flyspell-buffer)
  (add-hook 'parrot-click-hook #'parrot-start-animation)
  ;; Rotate parrot when buffer is saved
  (add-hook 'after-save-hook #'parrot-start-animation)
  ;;/Rotation function keybindings for evil users
  (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
  (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
  (add-hook 'mu4e-index-updated-hook #'parrot-start-animation)
  )


;; ** nyan-mode

(use-package nyan-mode
  :if window-system
  :hook
  (after-init . nyan-mode)
  :config
  (setq nyan-cat-face-number 4)
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  (nyan-start-animation))


;; **************************************************

;; * SESSION

;; create the savefile dir if it doesn't exist
(unless (file-exists-p tau-savefile-dir)
  (make-directory tau-savefile-dir))


;; ** desktop-mode


;; ** auto-save files

;; From ErgoEmacs:

;; "emacs has auto-save-mode, however, it's not what you think.
;; Emacs's auto-save-mode periodically saves a copy of your file with the name #filename#.

;; When you save the file, those #files# are deleted automatically. In case of crash or electricity outage, when you open a file afterward, emacs will detect those #files# and ask if you want to recover."

;; (use-package auto-save
;;   :ensure nil
;;   :config
(setq auto-save-default nil)  ;; stop creating those #auto-save# files
;;  )

;; ** savehist

;; Save minibuffer stories and others (by setting savehist-additional-variables)


(use-package savehist
  :ensure nil
  :config
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables
    '(kill-ring
       search-ring
       regexp-search-ring
       last-kbd-macro
       kmacro-ring
       shell-command-history))
  (savehist-mode)
  )


;; **************************************************

;; ** dashboard

;; DashboardPac
(use-package dashboard
  :ensure t
  :demand
  :diminish (dashboard-mode page-break-lines-mode)
  ;;:bind ("C-z d" . open-dashboard)
  :custom
  ;; Set the banner
  (dashboard-startup-banner 'logo) ;; values: ('oficial, 'logo, 1, 2, 3, or "path/to/image.png")
  ;; Set the title
  (dashboard-banner-logo-title
    (format "Emacs ready in %.2f seconds with %d garbage collections."
      (float-time
        (time-subtract after-init-time before-init-time)) gcs-done))
  ;; Set dashboard itens
  (dashboard-items '((recents  . 7)
                      (bookmarks . 7)
                      (agenda . 5)))
  ;; Set dashboard as the inicial buffer on startup
  (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  ;; Content is not centered by default. To center, set
  (dashboard-center-content t)
  ;; To disable shortcut "jump" indicators for each section, set
  (dashboard-show-shortcuts t)
  ;; To add icons to the widget headings and their items:
  (dashboard-set-heading-icons t)
  ;; Set file icons
  (dashboard-set-file-icons t)
  ;; set widgets to show
  (setq dashboard-items '((recents  . 5)
                           (bookmarks . 5)
                           (projects . 5)
                           (agenda . 5)
                           (registers . 5)))
  ;; To show navigator below the banner
  (dashboard-set-navigator t)
  ;;To show info about the packages loaded and the init time:
  (dashboard-set-init-info t)
  ;; Set the navigator buttons
  (dashboard-navigator-buttons
    (if (featurep 'all-the-icons)
      `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust -0.05)
           "dotfiles @ github" "Browse personal dotfiles"
           (lambda (&rest _) (browse-url "https://github.com/gugutz/dotfiles")))
          (,(all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.1)
            "Configuration" "" (lambda (&rest _) (edit-configs)))
          (,(all-the-icons-faicon "cogs" :height 1.0 :v-adjust -0.1)
            "Update packages" "" (lambda (&rest _) (auto-package-update-now)))))
      `((("" "M-EMACS" "Browse M-EMACS Homepage"
           (lambda (&rest _) (browse-url "https://github.com/gugutz/dotfiles")))
          ("" "Configuration" "" (lambda (&rest _) (edit-configs)))
          ("" "Update" "" (lambda (&rest _) (auto-package-update-now)))))))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Love LetterTW" :height 123))))
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :config
  (dashboard-modify-heading-icons '((recents . "file-text")
                                     (bookmarks . "book")))
  (dashboard-setup-startup-hook)
  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows)))
;; -DashboardPac

;; PBLPac
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
;; -PBLPac


;; ** toggle window transparency


(defun tau/toggle-window-transparency ()
  "Cycle the frame transparency from default to transparent."
  (interactive)
  (let ((transparency 85)
         (opacity 100))
    (if (and (not (eq (frame-parameter nil 'alpha) nil))
          (< (frame-parameter nil 'alpha) opacity))
      (set-frame-parameter nil 'alpha opacity)
      (set-frame-parameter nil 'alpha transparency))))

(global-set-key (kbd "M-<f12> t") 'tau/toggle-window-transparency)

;; ** nav-flash

(use-package nav-flash
  :ensure t
  :config
  (nav-flash-show)
  )

;; ** pulse


;; Visualize TAB, (HARD) SPACE, NEWLINE
;; Pulse current line
(use-package pulse
  :ensure nil
  :preface
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (if (fboundp 'xref-pulse-momentarily)
      (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))
  :hook (((dumb-jump-after-jump
            imenu-after-jump) . my-recenter-and-pulse)
          ((bookmark-after-jump
             magit-diff-visit-file
             next-error) . my-recenter-and-pulse-line))
  :init
  (dolist (cmd '(recenter-top-bottom
                  other-window ace-window windmove-do-window-select
                  pager-page-down pager-page-up
                  symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))
  (dolist (cmd '(pop-to-mark-command
                  pop-global-mark
                  goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse))
  )


;; ** prettify symbols

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols. See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
    '(
       ("lambda" . 955)
       ("delta" . 120517)
       ("epsilon" . 120518)
       ("->" . 8594)
       ("<=" . 8804)
       (">=" . 8805)
       )))
;;(global-prettify-symbols-mode 1)
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)



;; * Highlights

;; ** show paren mode

;; Highlight (by bolding) the matching parenthesis

(use-package paren
  :ensure nil
  :defer t
  :hook
  (prog-mode . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  (show-paren-mismatch ((nil (:background "red" :foreground "black")))) ;; :box t
  :custom
  (show-paren-delay 0.1)
  (show-paren-style 'mixed)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode +1)
  )

;; ** color identifiers mode

(use-package color-identifiers-mode
  :ensure t
  :defer t
  ;;:config
  ;;(add-hook 'after-init-hook 'global-color-identifiers-mode)
  ;; the following code disabled highlighting for all other keywords and only highlights and color variables
  ;;   (defun myfunc-color-identifiers-mode-hook ()
  ;;     (let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face font-lock-negation-char-face font-lock-regexp-grouping-construct font-lock-regexp-grouping-backslash)))
  ;;       (dolist (face faces)
  ;;         (face-remap-add-relative face '((:foreground "" :weight normal :slant normal)))))
  ;;     (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))
  ;;     (face-remap-add-relative 'font-lock-comment-face '((:slant italic)))
  ;;     (face-remap-add-relative 'font-lock-builtin-face '((:weight bold)))
  ;;     (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
  ;;     (face-remap-add-relative 'font-lock-function-name-face '((:slant italic)))
  ;;     (face-remap-add-relative 'font-lock-string-face '((:slant italic)))
  ;;     (face-remap-add-relative 'font-lock-constant-face '((:weight bold))))
  ;;   (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)
  )


;; ** Highlighting numbers


(use-package highlight-numbers
  :ensure t
  :defer t
  :hook
  (prog-mode . highlight-numbers-mode)
  )

;; ** Highlighting operators

(use-package highlight-operators
  :ensure t
  :defer t
  :hook
  (prog-mode . highlight-operators-mode)
  )

;; ** Highlighting escape sequences


(use-package highlight-escape-sequences
  :ensure t
  :defer t
  :hook
  (prog-mode . hes-mode)
  )


;; ** Highlighting parentheses

;; This mode highlights (coloring) the current pair in which the point (cursor) is

(use-package highlight-parentheses
  :ensure t
  :defer t
  :diminish
  :hook
  (prog-mode . highlight-parentheses-mode)
  )

;; ** diff-hl (highlights uncommited diffs in bar aside from the line numbers)

(use-package diff-hl
  :ensure t
  :defer t
  :custom-face
  ;; Better looking colours for diff indicators
  (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  (diff-hl-change ((t (:background "#3a81c3"))))
  (diff-hl-insert ((t (:background "#7ccd7c"))))
  (diff-hl-delete ((t (:background "#ee6363"))))
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-mode)
  :custom
  (diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (diff-hl-side 'left)
  (diff-hl-margin-side 'left)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t)
  ;; Set fringe style
  (setq-default fringes-outside-margins t)
  (diff-hl-margin-mode 1) ;; show the indicators in the margin
  (diff-hl-flydiff-mode 1) ;;  ;; On-the-fly diff updates

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


;; ** Highlight TODO

;; Basic support todos.
;; By default these include:
;; TODO NEXT THEM PROG OKAY DONT FAIL DONE NOTE KLUDGE HACK TEMP FIXME
;; and any sequence of X's or ?'s of length at least 3: XXX, XXXX, XXXXX, …, ???, ????, ????, ….


;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  :ensure t
  :init
  ;; (add-hook 'text-mode-hook (lambda () (hl-todo-mode t)))
  :config
  ;; Adding a new keyword: TEST.
  (add-to-list 'hl-todo-keyword-faces '("TODO" . "#ff3300"))
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "#dc8cc3"))
  (add-to-list 'hl-todo-keyword-faces '("NOTE" . "#ffff00"))
  (add-to-list 'hl-todo-keyword-faces '("DONE" . "#00ff00"))
  )



;; ** Rainbow Delimiters

;; This highlights matching parentheses by coloring them acording to their depth
;; Specially helpful for editing lisp code


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3)
  )


;; ** rainbow mode

;; : Colorize hex, rgb and named color codes

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish
  :hook
  ((prog-mode css-mode web-mode) . rainbow-mode)
  )


;; ** Highlight lines

;; : built-in package

(use-package hl-line
  :ensure nil
  :defer t
  :config
  (global-hl-line-mode)
  )


;; ** Highlight columns

(use-package col-highlight
  :load-path "packages/col-highlight"
  :ensure nil
  :defer t
  :config
  (col-highlight-toggle-when-idle)
  (col-highlight-set-interval 2)
  )

;; ** highlight indent guides

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :diminish
  :hook
  ((prog-mode yaml-mode toml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character) ; column
  )

;; * crux

;; CruxPac
(use-package crux
  :ensure t
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-x 4 t" . crux-transpose-windows)
  ("C-x K" . crux-kill-other-buffers)
  ("C-k" . crux-smart-kill-line)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "s-r") #'crux-recentf-find-file)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  )


;; **************************************************
;;
;;; * Scrolling settings

;;; DOOM-EMACS SCROLLING SETTING
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
(add-hook 'term-mode-hook (lambda () (hscroll-margin 0)))
(add-hook 'eshell-mode-hook (lambda () (hscroll-margin 0)))

(when IS-MAC
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
    mac-mouse-wheel-smooth-scroll nil))


;;; M-EMACS SCROLLING SETTING

;; ;; General Vertical Scrolling Settings
;; (setq scroll-step 1) ;; one line at a time
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-preserve-screen-position t)  ;; centered screen scrolling
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)

;; ;; Mouse Scroll
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; shift + mouse scroll moves 1 line at a time, instead of 5 lines
;; (setq mouse-wheel-scroll-amount '(3 ((control) . 6))) ;; hold Control for 5 lines at a time
;; (setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
;; (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line,  don't accelerate scrolling

;; ;; Horizontal Scroll
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)


;; **************************************************

;; * MOUSE SETTINGS

;; on linux, make right button show char info (of current cursor position, not clicked point)
(when (string-equal system-type "gnu/linux") ; linux
  (global-set-key (kbd "<mouse-3>") 'describe-char)
  )

;; **************************************************

;; * Global minor modes

;; **************************************************
;;
  ;;; ** which-key

(use-package which-key
  :ensure t
  :diminish
  :defer t
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (setq  which-key-sort-uppercase-first nil)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 6)
  (setq which-key-side-window-slot -10)
  (setq which-key-max-description-length 20)
  (setq which-key-idle-delay 0.2)
  ;; (setq which-key-max-display-columns 6)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook (lambda () (line-spacing 3)))
  )


;; **************************************************
;;
  ;;; ** key-frequency

(use-package keyfreq
  :ensure t
  :defer t
  :hook (after-init . keyfreq-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )


;; * Local minor modes
;; ** smartparens

(use-package smartparens
  :ensure t
  :defer t
  :diminish
  :hook
  (prog-mode . smartparens-mode)
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
    sp-highlight-wrap-overlay nil
    sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook 'minibuffer-setup-hook
    (defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression',
`pp-eval-expression' or `evil-ex'."
      (when (memq this-command '(eval-expression pp-eval-expression evil-ex))
        (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar doom-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
    (defun doom-enable-smartparens-mode-maybe-h ()
      (when doom-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'doom-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
    (defun doom-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local doom-buffer-smartparens-mode t)
        (turn-off-smartparens-mode))))

  (smartparens-global-mode +1)
  )


;; *** evil-smartparens helps avoid conflicts between evil and smartparens

(use-package evil-smartparens
  :ensure t
  :diminish
  :hook
  (smartparens-enabled . evil-smartparens-mode)
  )


;; ** Smartscan mode
;; : Usage:
;; : M-n and M-p move between symbols
;; : M-' to replace all symbols in the buffer matching the one under point
;; : C-u M-' to replace symbols in your current defun only (as used by narrow-to-defun.)

(use-package smartscan
  :ensure t
  :defer t
  )


;; ** editorconfig

(use-package editorconfig
  :ensure t
  :defer t
  :diminish
  )


;; * multiple cursors

(use-package multiple-cursors
  :after evil
  :defer t
  ;; step 1, select thing in visual-mode (OPTIONAL)
  ;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
  ;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
  ;; step 4, press s or S to start replace
  ;; step 5, press C-g to quit multiple-cursors
  :bind
  ("M-u" . hydra-multiple-cursors/body)
  (:map evil-visual-state-map
    ("C-d" . mc/mark-next-like-this)
    ("C-a" . mc/mark-all-like-this)
    )
  :config
  (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)
  )


;; * quickrun (compile and execute code)

(use-package quickrun
  :ensure t
  :defer t
  :bind
  ("C-<f5>" . quickrun)
  ("M-<f5>" . quickrun-shell)
  )


;; **************************************************

;; * LaTeX


(use-package tex
  :ensure auctex
  :mode ("\\.tex" . latex-mode)
  :defer t
  :hook
  (LaTeX-mode . rainbow-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
    TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :config
  (setq TeX-PDF-mode t) ;; compile to PDF by default
  (setq org-export-with-smart-quotes t) ;; convert quotes to LaTeX smartquotes on export

  (add-hook 'LaTeX-mode-hook
    (lambda ()
      (turn-on-reftex)
      (setq reftex-plug-into-AUCTeX t)
      (reftex-isearch-minor-mode)
      (setq TeX-PDF-mode t)
      (setq TeX-source-correlate-method 'synctex)
      (setq TeX-source-correlate-start-server t)))
  )



(use-package auctex-latexmk
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup)
  :hook
  ;; example of lambda usage in :hooks
  (LaTeX-mode . (lambda () (TeX-fold-mode t)))
  )




(use-package company-auctex
  :ensure t
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'company-auctex-init)
  )



;; ** Add the beamer presentation class template to org

(with-eval-after-load 'org
  '(add-to-list 'org-latex-classes
     '("beamer"
        "\\documentclass\[presentation\]\{beamer\}"
        ("\\section\{%s\}" . "\\section*\{%s\}")
        ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
        ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))



;; ** Add the memoir class template to org

;; The Sections and Heading Levels gets configured as follows:

;; | Division       | <c>Level | <c>org-equivalent |
;; | \book          |       -2 | *                 |
;; | \part          |       -1 | **                |
;; | \chapter       |        0 | ***               |
;; | \section       |        1 | ****              |
;; | \subsection    |        2 | *****             |
;; | \subsubsection |        3 | ******            |
;; | \paragraph     |        4 | *******           |
;; | \subparagraph  |        5 | ********          |



                                        ;(add-to-list 'org-latex-classes
                                        ;        '("memoir"
                                        ;          "\\documentclass\[a4paper\]\{memoir\}"
                                        ;          ("\\book\{%s\}" . "\\book*\{%s\}")
                                        ;          ("\\part\{%s\}" . "\\part*\{%s\}")
                                        ;          ("\\chapter\{%s\}" . "\\chapter*\{%s\}")
                                        ;          ("\\section\{%s\}" . "\\section*\{%s\}")
                                        ;          ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                                        ;          ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))
                                        ;)


;; ** Add abntex2 class to org list of latex classes
;; This class is based on the Memoir class
;; The Sections and Heading Levels gets configured as follows:

;; | Division       | <c>Level | <c>org-equivalent |
;; | \part          |       -1 | *                 |
;; | \chapter       |        0 | **                |
;; | \section       |        1 | ***               |
;; | \subsection    |        2 | ****              |
;; | \subsubsection |        3 | *****             |
;; | \paragraph     |        4 | ******            |
;; | \subparagraph  |        5 | *******           |

                                        ;(add-to-list 'org-latex-classes
                                        ;             '("abntex2"
                                        ;               "\\documentclass{abntex2}"
                                        ;               ("\\part{%s}" . "\\part*{%s}")
                                        ;               ("\\chapter{%s}" . "\\chapter*{%s}")
                                        ;               ("\\section{%s}" . "\\section*{%s}")
                                        ;               ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ;               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ;               ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
                                        ;               ("\\paragraph{%s}" . "\\paragraph*{%s}"))
                                        ;)

(with-eval-after-load 'org
  '(add-to-list 'org-latex-classes
     '("abntex2"
        "\\documentclass{abntex2}"
        ;; ("\\chapter{%s}" . "\\chapter*{%s}")
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}"))))


;; **************************************************

;; * WEBDEV

;; ** HTML

(use-package sgml-mode
  :ensure nil
  :defer t
  :hook
  ;;(html-mode . (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))
  (sgml-mode . aggressive-indent-mode)
  (sgml-mode . rainbow-mode)
  (sgml-mode . emmet-mode)
  :init
  ;; (setq sgml-basic-offset 4)
  :config
  ;;(add-hook 'html-mode-hook
  ;;  (lambda ()
  ;;    (set (make-local-variable 'sgml-basic-offset) 4)))
  )


;; ** CSS

(use-package css-mode
  :ensure t
  :defer t
  :mode "\\.css\\'"
  :preface
  (defun setup-css-mode ()
    (interactive)
    (message "Trying to setup css-mode for buffer " (buffer-file-name))
    (flycheck-mode 1)
    (prettier-js-mode 1)
    (emmet-mode 1)
    (aggressive-indent-mode 1)
    (editorconfig-mode 1)
    )
  :hook
  (css-mode . setup-css-mode)
  ;;(css-mode . aggressive-indent-mode)
  ;;(css-mode . prettier-js-mode)
  ;;(css-mode . emmet-mode)
  :init
  (setq css-indent-offset 2)
  )


;; ** SCSS

(use-package scss-mode
  :ensure t
  :defer t
  ;; this mode doenst load using :mode from use-package, dunno why
  :mode ("\\.scss\\'")
  :init
  (setq scss-compile-at-save 'nil)
  :hook
  (scss-mode . prettier-js-mode)
  (scss-mode . rainbow-mode)
  (scss-mode . aggressive-indent-mode)
  (scss-mode . emmet-mode)
  :config
  (autoload 'scss-mode "scss-mode")
  (setq scss-compile-at-save 'nil)
  (add-to-list 'auto-mode-alist '("\\.scss$\\'" . scss-mode))
  )


;; ** Web-Mode

(use-package web-mode
  :defer t
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
  :preface
  (defun setup-web-mode ()
    (interactive)
    (message "Trying to setup web-mode for buffer " (buffer-file-name))
    (flycheck-mode 1)
    (eldoc-mode 1)
    (emmet-mode 1)
    (prettier-js-mode 1)
    (editorconfig-mode 1)
    (smartscan-mode 1)
    )
  :hook
  (web-mode . setup-web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-block-face t)
  (web-mode-enable-part-face t)
  :config
  ;; TSX
  (add-hook 'web-mode-hook
    (lambda ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode))))
  ;; Template
  (setq web-mode-engines-alist
    '(("php"    . "\\.phtml\\'")
       ("blade"  . "\\.blade\\.")))
  ;;---------------------------------------
  )

;; ** js2-mode

(use-package js2-mode
  :after flycheck company
  :defer t
  :mode
  ("\\.js$" . js2-mode)
  :hook
  (js2-mode . flycheck-mode)
  (js2-mode . add-node-modules-path)
  (js2-mode . rainbow-mode)
  (js2-mode . color-identifiers-mode)
  (js2-mode . prettier-js-mode)
  (js2-mode . aggressive-indent-mode)
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
  :config
  ;;=======================================
  ;; Flycheck Setup for JavaScript
  ;; add eslint to list of flycheck checkers
  ;;---------------------------------------
  (setq flycheck-checkers '(javascript-eslint))
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  ;; set modes that will use ESLint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  )

;; ** js2-refactor

(use-package js2-refactor
  :ensure t
  :defer t
  :after js2-mode
  :hook
  (js2-mode . js2-refactor-mode)
  :bind
  (:map js2-mode-map
    ("C-k" . js2r-kill)
    ("C-c h r" . js2-refactor-hydra/body))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  (defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
      ("ee" js2r-expand-node-at-point)
      ("cc" js2r-contract-node-at-point)
      ("ef" js2r-extract-function)
      ("em" js2r-extract-method)
      ("tf" js2r-toggle-function-expression-and-declaration)
      ("ta" js2r-toggle-arrow-function-and-expression)
      ("ip" js2r-introduce-parameter)
      ("lp" js2r-localize-parameter)
      ("wi" js2r-wrap-buffer-in-iife)
      ("ig" js2r-inject-global-in-iife)
      ("ag" js2r-add-to-globals-annotation)
      ("ev" js2r-extract-var)
      ("iv" js2r-inline-var)
      ("rv" js2r-rename-var)
      ("vt" js2r-var-to-this)
      ("ao" js2r-arguments-to-object)
      ("ti" js2r-ternary-to-if)
      ("sv" js2r-split-var-declaration)
      ("ss" js2r-split-string)
      ("uw" js2r-unwrap)
      ("lt" js2r-log-this)
      ("dt" js2r-debug-this)
      ("sl" js2r-forward-slurp)
      ("ba" js2r-forward-barf)
      ("k" js2r-kill)
      ("q" nil))
    )

;; ** PrettierJS

(use-package prettier-js
  :ensure t
  :defer t
  :custom
  (prettier-js-show-errors 'buffer) ;; options: 'buffer, 'echo or nil
  :config
  )

;; ** Emmet

(use-package emmet-mode
  :ensure t
  :defer t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  (setq emmet-expand-jsx-className? nil) ;; use emmet with JSX markup
  )


;; ** xref-js2

;; | M-. | Jump  to definition                         |
;; | M-? | Jump to references                          |
;; | M-, | Pop back  to where  M-.  was  last  invoked |

(use-package xref-js2
  :ensure t
  :defer t
  :config
  ;;(setq xref-js2-search-program 'rg)

  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  )

;; ** add-node-modules-path

(use-package add-node-modules-path
  :ensure t
  :defer t
  )

;; ** js-import

(use-package js-import
  :ensure t
  :defer t
  )

;; **************************************************

;; ** Typescript

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode)
    ("\\.tsx\\'" . typescript-mode))
  :preface
  (defun setup-typescript-mode ()
    (interactive)
    (message "Trying to setup typescript-mode for buffer")
    (flycheck-mode 1)
    (eval-after-load 'flycheck
      '(flycheck-add-mode 'typescript-tslint 'typescript-mode))
    (eldoc-mode 1)
    (yas-minor-mode 1)
    (add-node-modules-path)
    (editorconfig-mode 1)
    (prettier-js-mode 1)
    (dumb-jump-mode 1)
    (hl-todo-mode 1)
    (smartscan-mode 1)
    )
  :hook
  (typescript-mode . setup-typescript-mode)
  )

;; ** RJSX Mode

(use-package rjsx-mode
  :ensure t
  :defer t
  :after js2-mode
  :mode
  ("\\.jsx$" . rjsx-mode)
  ("components/.+\\.js$" . rjsx-mode)
  :hook
  (rjsx-mode . add-node-modules-path)
  (rjsx-mode . prettier-js-mode)
  :config
  ;; auto register for JS files that are inside a `components' folder
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  )


;; ** Angular

;; *** Angular Open Counterpart (taken from ng2-mode)


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

(global-set-key (kbd "C-x a o") #'angular-open-counterpart)


;; **************************************************

;; * Ruby

(use-package ruby-mode
  :defer t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :ensure-system-package
  ((rubocop     . "gem install rubocop")
    (ruby-lint   . "gem install ruby-lint")
    (ripper-tags . "gem install ripper-tags")
    (pry         . "gem install pry"))
  :functions inf-ruby-keys
  :hook
  (ruby-mode . subword-mode)
  (ruby-mode . eldoc-mode)
  (enh-ruby-mode . lsp)
  :config
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))

  ;; Switch the compilation buffer mode with C-x C-q (useful
  ;; when interacting with a debugger)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)

  (add-hook 'ruby-mode-hook
    (lambda ()
      (hs-minor-mode 1) ;; Enables folding
      (modify-syntax-entry ?: "."))) ;; Adds ":" to the word definition
  )


;; ** ruby refactor

;; Functions to help with refactoring
(use-package ruby-refactor
  :ensure t
  :defer t
  :hook
  (ruby-mode . ruby.refactor-mode-launch)
  )


;; ** ruby hash syntax

;; Easily toggle ruby's hash syntax


(use-package ruby-hash-syntax
  :ensure t
  :defer t
  )


;; ** ruby-tools


(use-package ruby-tools
  :ensure t
  :defer t
  )


;; ** rspec-mode

;; Support for running rspec tests


(use-package rspec-mode
  :ensure t
  :defer t
  )


;; ** ruby-blocks

;; Highlights ruby def/end blocks
;; Unfortunatelly this package has to be installed manually, as its not available on MELPA


(use-package ruby-block
  :disabled
  :ensure t
  :defer t
  :init
  ;; do overlay
  (setq ruby-block-highlight-toggle 'overlay)
  ;; display to minibuffer
  (setq ruby-block-highlight-toggle 'minibuffer)
  ;; display to minibuffer and do overlay
  (setq ruby-block-highlight-toggle t)
  :config
  (ruby-block-mode t)
  )


;; ** ruby-extra-highlights


(use-package ruby-extra-highlight
  :ensure t
  :defer t
  :hook
  (ruby-mode . ruby-extra-highlight-mode)
  )


;; **************************************************

;; * Go


(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  )


;; ** company-go

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go))
  )

;; **************************************************

;; * YAML

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  )


;; * TOML

(use-package toml-mode
  :ensure t
  :defer t
  :mode "\\.toml\\'"
  )


;; * format-all


(use-package format-all
  :ensure t
  :defer t
  :bind ("C-c C-f" . format-all-buffer)
  )

;; * JSON Mode

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode


(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2)

  ;;Flycheck setup for JSON Mode
  ;; disable json-jsonlist checking for json files
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(json-jsonlist))))
)



;; * Docker


(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "\\Dockerfile\\'"
  )


;; * Helper functions

;; ** Duplicate line


;; (defun duplicate-line()
;;   "Duplicate current line."
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;;   (open-line 1)
;;   (next-line 1)
;;   (yank))

;; shorter version
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

;; ** Move Lines Up and Down

(defun move-line-down ()
  "Move line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "Move line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") 'move-line-down)
(global-set-key (kbd "C-S-k") 'move-line-up)



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init)
;;; init.el ends here
