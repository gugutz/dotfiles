;; -*- lexical-binding: t; -*-

;; FROM DOOM EMACS:
;; Use lexical-binding everywhere

;; Add ;; -*- lexical-binding: t; -*- to the top of your elisp files. This can break code if you’ve written it to depend on undeclared dynamic variables, but I’ve designed Doom not to.

;; This buys a small improvement in performance, but every little bit helps.
;; source: https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

;; **************************************************
;;
;; Emacs Initialization
;; source: https://raw.githubusercontent.com/gilbertw1/emacs-literate-starter/master/emacs.org


;; We're going to increase the gc-cons-threshold to a very high number to decrease the load and compile time.
;; We'll lower this value significantly after initialization has completed. We don't want to keep this value
;; too high or it will result in long GC pauses during normal usage.

;; Code
(setq gc-cons-threshold most-positive-fixnum ;; 2^61 bytes
  gc-cons-percentage 0.6)

;; Disable certain byte compiler warnings to cut down on the noise. This is a personal choice and can be removed if you would like to see any and all byte compiler warnings.

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;; Unset file-name-handler-alist temporarily.
;; Every file opened and loaded by Emacs will run through this list to check for a proper handler for the file, but during startup, it won’t need any of them. Doom doesn’t, at least.

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; re-set file-name-handler-alist when emacs finally starts
(add-hook 'emacs-startup-hook
  (setq file-name-handler-alist doom--file-name-handler-alist))
;; -------------------------------------



;; **************************************************
;;
;;; * PACKAGE MANAGEMENT

;; FROM DOOM EMACS:
;; Lazy load package management system(s)

;; Initializing package.el or straight.el at startup is expensive. We can save some time by delaying that initialization until we actually need these libraries (and load them only when we’re doing package management, e.g. when we run doom sync).

;; Among other things, doom sync does a lot for us. It generates concatenated autoloads files; caches expensive variables like caches load-path, Info-directory-list and auto-mode-alist; and preforms all your package management activities there – far away from your interactive sessions.

;; How exactly Doom accomplishes all this is a long story, so here is a boiled-down version you can use in your own configs (for package.el, not straight.el):

(defvar cache-file "~/.emacs.d/cache/autoloads")

;; (defun initialize ()
;;   (unless (load cache-file t t)
;;     (setq package-activated-list nil)
;;     (package-initialize)
;;     (with-temp-buffer
;;       (cl-pushnew user-emacs-directory load-path :test #'string=)
;;       (dolist (desc (delq nil (mapcar #'cdr package-alist)))
;;         (let ((load-file-name (concat (package--autoloads-file-name desc) ".el")))
;;           (when (file-readable-p load-file-name)
;;             (condition-case _
;;               (while t (insert (read (current-buffer))))
;;               (end-of-file)))))
;;       (prin1 `(setq load-path ',load-path
;;                 auto-mode-alist ',auto-mode-alist
;;                 Info-directory-list ',Info-directory-list)
;;         (current-buffer))
;;       (write-file (concat cache-file ".el"))
;;       (byte-compile-file cache-file))))

;; (initialize)

;; You’ll need to delete cache-files any time you install, remove, or update a new package. You could advise package-install and package-delete to call initialize when they succeed, or make initialize interactive and call it manually when necessary. Up to you!
;; -------------------------------------



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


;; Tell =use-package= to always defer loading packages unless explicitly told otherwise. This speeds up initialization significantly as many packages are only loaded later when they are explicitly used.

(setq use-package-always-defer t
  use-package-verbose t)
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
;; Next we are going to require =package.el= and add our additional package archives, 'melpa' and 'org'.
;; Afterwards we need to initialize our packages and then ensure that =use-package= is installed, which
;; we promptly install if it's missing. Finally we load =use-package= and tell it to always install any
;; missing packages.

;; Note that this entire block is wrapped in =eval-when-compile=. The effect of this is to perform all
;; of the package initialization during compilation so that when byte compiled, all of this time consuming
;; code is skipped. This can be done because the result of byte compiling =use-package= statements results
;; in the macro being fully expanded at which point =use-package= isn't actually required any longer.

;; Since the code is automatically compiled during runtime, if the configuration hasn't already been
;; previously compiled manually then all of the package initialization will still take place at startup.

(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  (package-initialize)

  ;; load use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))
;; -------------------------------------

;;
;; OTHER SETTINGS

(use-package diminish :demand t)



;; ##################################################
;;
;;; Emacs settings
;;
;; ##################################################

;; *********************************-
;;; General

;; Allow access from emacsclient
;; disable because i dont use server and emacsclient anymore
;; (unless (or (daemonp) (server-running-p))
;;   (server-start))

(setq ring-bell-function 'ignore) ;; Disable the annoying Emacs bell ring (beep)

(blink-cursor-mode 0) ;; dont blink the cursor

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

(setq kill-do-not-save-duplicates t) ;; Eliminate duplicates in the kill ring.

(setq next-line-add-newlines t) ;; C-n insert newlines if the point is at the end of the buffer.


;; *********************************-
;;
;;; Indentation

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

;; *********************************-
;;; Startup

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t)
(setq  inhibit-startup-echo-area-message user-login-name)
(setq  inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)
(setq  initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; start with init.el
(setq initial-buffer-choice "~/dotfiles/emacs.d/init.el")

;; indicate empty lines in the fringe with --
(setq-default indicate-empty-lines t)


;; **********************************
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
(setq vc-follow-symlinks t)

;; Prevent emacs to create lockfiles (.#files#)
(setq create-lockfiles nil)

;; stop creating those #auto-save# files
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list")
(setq auto-save-default nil)

;; disable emacs's automatic backup~ file
(setq make-backup-files nil)

;; dont ask confirmation to kill processes
(setq confirm-kill-processes nil)



;; Prevent emacs from writing several files in the config folder
(setq abbrev-file-name             (concat config-dir "abbrev.el"))
(setq async-byte-compile-log-file  (concat cache-dir "async-bytecomp.log"))
(setq bookmark-default-file        (concat config-dir "bookmarks"))
(setq custom-file                  (concat config-dir "custom.el"))
(setq tramp-auto-save-directory    (concat config-dir "tramp-auto-save/"))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-persistency-file-name  (concat config-dir "tramp-persistency.el"))
(setq url-cache-directory          (concat config-dir "url/"))
(setq url-configuration-directory  (concat config-dir "url/"))
(setq recentf-save-file "~/.emacs.d/config/recentf")


;; **********************************
;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)


;; **********************************
;;
;;; Minibuffer settings

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


;; ##################################################
;;
;;; PERFORMANCE OPTIMIZATIONS
;;
;; ##################################################

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


;; ##################################################
;;
;;; EMACS SESSION AND STATE
;;
;; ##################################################

;; *********************************-
;;
;; Save Place

;; remember where the cursor was when opening files

(use-package saveplace
  :ensure nil
  :demand t;; built-in
  :defer t
  :init
  (eval-when-compile
    (save-place-mode 1))
  (setq-default save-place t)
  ;; dont clutter the emacs folder. save somewhere else
  (setq save-place-file "~/.emacs.d/config/places")
  )

;; ##################################################
;;
;;; Improvements of Emacs native functions
;;
;; ##################################################
;;
;; C-k kills current buffer without having to select which buffer

;; prompt only if there are unsaved changes.
(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  )

(global-set-key (kbd "C-x k") #'kill-current-buffer)

;; refresh buffer with F5
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))


;; ##################################################
;;
;;; GC magic hack (used in doom-emacs)


(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1)
  )

;; ##################################################
;;
;;; Evil

(use-package evil
  :demand t
  :custom
  ;; change cursor color according to mode
  (evil-emacs-state-cursor '("#ff0000" box))
  (evil-motion-state-cursor '("#FFFFFF" box))
  (evil-normal-state-cursor '("#00ff00" box))
  (evil-visual-state-cursor '("#abcdef" box))
  (evil-insert-state-cursor '("#e2f00f" bar))
  (evil-replace-state-cursor '("#ff0000" hbar))
  (evil-operator-state-cursor '("#ff0000" hollow))
  ;; evil in modeline
  (evil-mode-line-format '(before . mode-line-front-space)) ;; move evil tag to beginning of modeline
  :config
  (evil-mode 1)
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

;; *********************************
;; evil-commentary

(use-package evil-commentary
  :demand t
  :after evil
  :diminish
  :config
  (evil-commentary-mode)
  )

;; *********************************
;; evil-surround
(use-package evil-matchit
  :demand t
  :config
  (global-evil-matchit-mode 1)
  )

;; *********************************
;; evil-surround

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
  :hook
  (prog-mode-hook . evil-surround-prog-mode-hook-setup)
  (js2-mode-hook . evil-surround-js-mode-hook-setup)
  (emacs-lisp-mode-hook . evil-surround-emacs-lisp-mode-hook-setup)
  (org-mode-hook . evil-surround-org-mode-hook-setup)
  :config
  (global-evil-surround-mode 1)
  )

;; **************************************************
;;
;;; Ivy

(use-package ivy
  :demand t
  :defer t
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
  (eval-when-compile
    (ivy-mode 1)
    ;; Add recent files and bookmarks to the ivy-switch-buffer
    (setq ivy-use-virtual-buffers t)
    ;;Displays the current and total number in the collection in the prompt
    (setq enable-recursive-minibuffers t)
    ;; display an arrow on the selected item in the list
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

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
;;
;;; Counsel
(use-package counsel
  :demand t
  :defer t
  :after ivy
  :diminish counsel-mode
  :hook
  (ivy-mode . counsel-mode)
  )

;; ** Counsel integration for Projectile

(use-package counsel-projectile
  :demand t
  :defer t
  :after ivy counsel projectile
  :config
  (eval-when-compile
    (counsel-projectile-mode 1))
  )

;; *********************************
;;
;; Enhance M-x
;; Enhancement for `execute-extended-command'. Auto detects and uses ivy or ido, if installed
(use-package amx
  :demand t
  :defer t
  :after ivy
  :init
  (setq amx-save-file "~/.emacs.d/config/amx-items")
  :config
  (amx-mode)
  )

;; *********************************
;;
;; ivy-rich

(use-package ivy-rich
  :demand t
  :defer t
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
  :config
  ;; ivy-rich-mode needs to be called after `ivy-rich--display-transformers-list' is changed
  (eval-when-compile
    (ivy-rich-mode 1))
  )

;; *********************************
;;
;; ** ivy-posframe

;; Requires: Emacs >= 26

(use-package ivy-posframe
  :demand t
  :defer t
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
  :demand t
  :defer t
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
  :demand t
  :defer t
  :after ivy
  :config
  (eval-when-compile
    (ivy-prescient-mode))
  )

;; *********************************
;;
;; all the icons for ivy

(use-package all-the-icons-ivy
  :demand t
  :defer t
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
;;
;;
;;; avy
(use-package avy
  :demand t
  :defer t
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

;; **************************************************
;;
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize)
  )

;; *********************************
;;
;; goto-line-preview

(use-package goto-line-preview
  :demand t
  :defer t
  ;; :config
  ;; (global-set-key [remap goto-line] 'goto-line-preview)
  )

;; ################################################
;;
;; FILE / DIRECTORY NAVIGATION
;;
;; ################################################

;; **************************************************
;;
;;; Treemacs

(use-package treemacs
  :demand t
  :defer t
  :bind
  ("<f8>" . treemacs)
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-persist-file (expand-file-name "cache/treemacs-persist" user-emacs-directory))
  (treemacs-resize-icons 21)
  )

(use-package treemacs-evil
  :after treemacs evil
  :demand t)

(use-package treemacs-projectile
  :after treemacs projectile
  :demand t
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :demand t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :demand t
  )

(use-package treemacs-persp
  :after treemacs persp-mode
  :demand t
  :config (treemacs-set-scope-type 'Perspectives))


;; **************************************************

;; ** ranger

(use-package ranger
  :demand t
  :defer t
  :bind
  ("C-x C-j" . ranger)
  (:map evil-normal-state-map
    ("SPC f r" . ranger))
  :config
  (setq ranger-show-hidden t) ;; show hidden files
  )

;; **************************************************
;;
;; ** Dired

;; Make dired look like k

(use-package dired-k
  :demand t
  :defer t
  :after dired
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-dwin-target t)
  (add-hook 'dired-initial-position-hook #'dired-k)
  )

;; ** all the icons dired

(use-package all-the-icons-dired
  :demand t
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode)
  )



;; ################################################
;;
;; TEXT MANIPULATION
;;
;; ################################################

;; *********************************
;; ** Duplicate line

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
;;
;; expand region

;; smart selection of text

(use-package expand-region
  :demand t
  :defer t
  :bind
  ([(control shift iso-lefttab)] . er/expand-region)
  ;; ("C-=" . er/expand-region)
  )

;; *********************************
;;
;; ** subword-mode

;; change all cursor movement/edit commands to stop in-between the “camelCase” words.
;; subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.


(use-package subword
  :ensure nil
  :defer t
  :demand t
  :hook
  (js2-mode . subword-mode)
  (typescript-mode . subword-mode)
  )

;; *********************************
;;
;; ** superword-mode

;; treats text like “x_y” as one word. Useful for “snake_case”.
;; subword-mode and superword-mode are mutally exclusive. Turning one on turns off the other.

(use-package superword
  :ensure nil
  :demand t
  :defer t
  :hook
  (clojure-mode . superword-mode)
  (ruby-mode . superword-mode)
  (elixir-mode . superword-mode)
  (emacs-lisp-mode . superword-mode)
  )

;; *********************************
;;
;;; drag stuff / move text

(use-package drag-stuff
  :demand t
  :bind
  ("M-k" . drag-stuff-up)
  ("M-j" . drag-stuff-down)
  ("M-h" . drag-stuff-left)
  ("M-l" . drag-stuff-right)
  :config
  (drag-stuff-mode t)
  )

;; *********************************
;;
;; electric pair mode

(use-package electric-pair-mode
  :ensure nil
  :demand t
  :defer t
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

;; ;; *********************************
;; ;;
;; ;; ** smartparens

;; (use-package smartparens
;;   :demand t
;;   :defer t
;;   :diminish
;;   :hook
;;   (prog-mode . smartparens-mode)
;;   :config
;;   ;; (smartparens-global-mode +1)
;;   ;; Load default smartparens rules for various languages
;;   (require 'smartparens-config)
;;   (sp-pair "<" ">" :actions '(wrap))

;;   ;; Overlays are too distracting and not terribly helpful. show-parens does
;;   ;; this for us already (and is faster), so...
;;   (setq sp-highlight-pair-overlay nil)
;;   (setq sp-highlight-wrap-overlay nil)
;;   (setq sp-highlight-wrap-tag-overlay nil)
;;   (with-eval-after-load 'evil
;;     ;; But if someone does want overlays enabled, evil users will be stricken
;;     ;; with an off-by-one issue where smartparens assumes you're outside the
;;     ;; pair when you're really at the last character in insert mode. We must
;;     ;; correct this vile injustice.
;;     (setq sp-show-pair-from-inside t)
;;     ;; ...and stay highlighted until we've truly escaped the pair!
;;     (setq sp-cancel-autoskip-on-backward-movement nil))

;;   ;; dont try to escape quotes in strings
;;   (setq sp-escape-quotes-after-insert nil)

;;   (add-hook 'minibuffer-setup-hook
;;     (defun init-smartparens-in-minibuffer-maybe-h ()
;;       "Enable `smartparens-mode' in the minibuffer, during `eval-expression',
;; `pp-eval-expression' or `evil-ex'."
;;       (when (memq this-command '(eval-expression pp-eval-expression evil-ex))
;;         (smartparens-mode))))

;;   )

;; *********************************
;;
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
;;
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

;; ################################################
;;
;; CODE NAVIGATION
;;
;; ################################################

;; *********************************
;;
;; dumb-jump

(use-package dumb-jump
  :demand t
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



;; ##################################################
;;
;;; WHITESPACE

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Set the whitespace highlight color
(set-face-background 'trailing-whitespace "#f44545")

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ##################################################
;;
;;; Window Management

;; auto balance windows on opening and closing frames
(setq window-combination-resize t)

;; ** auto balance windows area
(global-set-key (kbd "C-M-+") 'balance-windows-area)



;; *********************************
;; Eyebrowse

(use-package eyebrowse
  :demand t
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
  :demand t
  :defer t
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
  :demand t
  :defer t
  :bind
  ("C-c r w" . rotate-window)
  ("C-c r l" . rotate-layout)
  ("M-S-O SPC" . rotate-layout)
  ("C-M-o" . hydra-frame-window/body)
  )



;; *********************************
;; ** windmove

;; (use-package windmove
;;   :demand t
;;   :defer t
;;   :config
;;   ;; use shift + arrow keys to switch between visible buffers
;;   ;; (windmove-default-keybindings)
;;   )

;; ##################################################
;;
;; ** FONTS AND ICONS

;; set default font

;; Find the first font in the list and use it
(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

;; define list of fonts to be used in the above function
;; the first one found will be used
(set-face-attribute 'default nil :font (font-candidate '
                                         "Hack-11:weight=normal"
                                         "Droid Sans Mono-11:weight=normal"
                                         "Consolas-10:weight=normal"
                                         "DejaVu Sans Mono-11:weight=normal"
                                         "Ubuntu Mono-10:weight=normal"
                                         ))


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



;; ##################################################
;;
;; PROJECTS

;; *********************************
;;;  Projectile

(use-package projectile
  :demand t
  :defer t
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


;; ##################################################
;;
;; AUTOCOMPLETE

;; *********************************
;;; Company

(use-package company
  :demand t
  :defer t
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

  (eval-when-compile
    (global-company-mode)
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    ;; show tooltip even for single candidates
    (setq company-frontends '(company-pseudo-tooltip-frontend
                               company-echo-metadata-frontend)))
  )

;; *********************************
;;; company box

(use-package company-box
  :demand t
  :defer t
  :after company
  :hook
  ((global-company-mode company-mode) . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (setq company-box-icons-all-the-icons
    (let ((all-the-icons-scale-factor 0.9))
      `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
         (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
         (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
         (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
         (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
         (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
         (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
         (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
         (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
         (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
         (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
         (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
         (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
         (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
         (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
         (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
         (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
         (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
         (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
         (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
         (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
         (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
         (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
         (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
         (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
         (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
         (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))
  )


;; *********************************
;; company quickHelp

;; Documentation popups for Company

(use-package company-quickhelp
  :demand t
  :defer t
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
;; lsp

(use-package lsp-mode
  :demand t
  :defer t
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
;; lsp ui

(use-package lsp-ui
  :demand t
  :defer t
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

(use-package company-lsp :demand t :defer t)


;; *********************************
;;; ** Yasnippets

(use-package yasnippet
  :demand t
  :defer t
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
(use-package yasnippet-snippets :demand t :defer t)


;; *********************************
;; FlyCheck linter

(use-package flycheck
  :demand t
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
  :demand t
  :defer t
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


;; ##################################################
;;
;;; HELP SYSTEM AND GUIDANCE
;;
;; ##################################################
;;

;; *********************************
;; helpfull

(use-package helpful
  :demand t
  :defer t
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
;; which-key

(use-package which-key
  :demand t
  :defer t
  :diminish
  :init
  (setq which-key-idle-secondary-delay 0.05) ;; make it refresh quicly between displays
  (setq which-key-idle-delay 0.3)
  (setq which-key-min-display-lines 6)
  (setq which-key-add-column-padding 1)
  :config
  (which-key-mode)
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-minibuffer)
  ;; (which-key-setup-side-window-right-bottom)
  ;; (add-hook 'which-key-init-buffer-hook (lambda () (line-spacing 3)))
  )

;; *********************************
;; keyfreq

(use-package keyfreq
  :demand t
  :defer t
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


;; ##################################################
;;
;; PACKAGES
;;
;; ##################################################


;; *********************************
;;
;; hippie expand

(use-package hippie-exp
  :demand t ;; builtin package
  :defer t
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
;;
;; restart emacs

(use-package restart-emacs :demand t :defer t)

;; *********************************
;; shell-pop

(use-package shell-pop
  :demand t
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

;; *********************************
;; auto revert mode

(use-package autorevert
  :ensure nil
  :demand t
  :defer t
  ;; :hook
  ;; revert buffers when their files/state have changed
  ;;(focus-in . revert-buffer)
  ;;(after-save . revert-buffer)
  ;;(switch-buffer . revert-buffer)
  ;;(switch-window . revert-buffer)
  :config
  (global-auto-revert-mode)
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
;; editorconfig

(use-package editorconfig
  :demand t
  :defer t
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
;; aggressive indent

(use-package aggressive-indent
  :demand t
  :defer t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (prog-mode . aggressive-indent-mode)
  (css-mode . aggressive-indent-mode)
  :config
  (setq aggressive-indent-comments-too t)
  )

;; *********************************
;;
;; conf mode

;; ** Use unix-conf-mode for .*rc files

(use-package conf-mode
  :defer t
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



;; ##################################################
;;
;; WEB DEVELOPMENT
;;
;; ##################################################
;;

;; *********************************
;;
;; Typescript Mode

(use-package typescript-mode
  :demand t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode)
    ("\\.tsx\\'" . typescript-mode))
  )

;; *********************************
;;
;; js2-mode

(use-package js2-mode
  :demand t
  :defer t
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
  :demand t
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
  :demand t
  :defer t
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
  :demand t
  :defer t
  ;; this mode doenst load using :mode from use-package, dunno why
  :mode ("\\.scss\\'")
  :init
  (setq scss-compile-at-save 'nil)
  :config
  ;; (autoload 'scss-mode "scss-mode")
  ;; (setq scss-compile-at-save 'nil)
  ;; (add-to-list 'auto-mode-alist '("\\.scss$\\'" . scss-mode))
  )



;; *********************************
;;
;; ** PrettierJS

(use-package prettier-js
  :demand t
  :defer t
  :hook
  (typescript-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (css-mode . prettier-js-mode)
  :custom
  (prettier-js-show-errors 'buffer) ;; options: 'buffer, 'echo or nil
  :config
  )

;; *********************************
;;
;; ** Emmet

(use-package emmet-mode
  :demand t
  :defer t
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

;; ##################################################
;;
;; LANGUAGES SETUP
;;
;; ##################################################

;; *********************************
;;
;; go mode


(use-package go-mode
  :ensure nil
  :demand t
  :defer t
  :commands emacs-lisp-mode
  :mode
  ("\\.go$" . go-mode)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  )


;; *********************************
;;
;; lisp mode
(use-package lisp-mode
  :ensure nil
  :demand t
  :defer t
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
;;
;; shell script mode

(use-package sh-script
  :ensure nil
  :demand t
  :defer t
  :hook
  (sh-mode . aggressive-indent-mode)
  (sh-mode . rainbow-mode)
  )


;; *********************************
;;
;; YAML

(use-package yaml-mode
  :demand t
  :defer t
  :mode
  ("\\.yaml\\'" "\\.yml\\'")
  )

;; *********************************
;;
;; vimrc

(use-package vimrc-mode
  :demand t
  :defer t
  :mode
  ("\\.vim\\(rc\\)?\\'" . vimrc-mode)
  )


;; ##################################################
;;
;; SIMULATE VSCODE FUNCTIONS
;;
;; ##################################################

;; *********************************
;;
;; magit

(use-package magit
  :demand t
  :defer t
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

;; evil-magit

(use-package evil-magit
  :demand t
  :defer t
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


;; ** diffview
;; View diffs side by side

(use-package diffview :demand t :defer t)

;; *********************************
;;
;; vc-msg

(use-package vc-msg
  :demand t
  :defer t
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
  :demand t
  :defer t
  :bind
  ("C-c g t" . git-timemachine-toggle)
  (:map evil-normal-state-map
    ("SPC g t" . git-timemachine-toggle))
  )

;; *********************************
;;
;; ** diff-hl (highlights uncommited diffs in bar aside from the line numbers)

(use-package diff-hl
  :demand t
  :defer t
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

;; Mode for .gitignore files.
(use-package gitignore-mode :demand t :defer t)
(use-package gitconfig-mode :demand t :defer t)
(use-package gitattributes-mode :demand t :defer t)

;; ##################################################
;;
;; SIMULATE VSCODE FUNCTIONS
;;
;; ##################################################



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


;; ##################################################
;;
;;; UI Settings / Enhancements
;;
;; ##################################################


;; Add `./themes' to path
(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")

;; Set the used theme
(setq my-theme 'vscode-dark-plus)

;;  Load the theme
(load-theme my-theme t)

(use-package all-the-icons
  :if window-system
  :demand t
  :commands
  (all-the-icons-octicon
    all-the-icons-faicon
    all-the-icons-fileicon
    all-the-icons-wicon
    all-the-icons-material
    all-the-icons-alltheicon)
  )

;; disable scroll bars from frames
(scroll-bar-mode -1)

;; Remove the menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil))


;; **************************************
;;
;; display-line-numbers

(use-package display-line-numbers
  :if (version<= "26.0.50" emacs-version)
  :ensure nil
  :demand t
  :config
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  ;; (setq linum-format "%4d \u2502 ") ; 4 chars and a space with solid line separator
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
  )


;; *********************************
;;
;; emacs 27 native tabs

(use-package tabbar
  :disabled
  :ensure nil
  :demand t
  :config
  (tab-bar-mode +1) ;; per-frame
  ;; (tab-line-mode +1) ;; per window
  )

;; *********************************
;;
;; Centaur tabs
(use-package centaur-tabs
  :demand t
  :defer t
  :config
  (centaur-tabs-mode t)
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
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (when (member "Arial" (font-family-list))
    (centaur-tabs-change-fonts "Arial" 130)))

;; *********************************
;;
;; vi tilde fringe

;; Displays tildes in the fringe on empty lines a la Vi.

(use-package vi-tilde-fringe
  :demand t
  :defer t
  :diminish
  :config
  (eval-when-compile
    (prog-mode . vi-tilde-fringe-mode)
    (text-mode . vi-tilde-fringe-mode))
  )

;; *********************************
;;
;; Solaire mode

(use-package solaire-mode
  :demand t
  :defer t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (eval-when-compile
    (solaire-global-mode +1)
    (solaire-mode-swap-bg))
  )


;; ##################################################
;; ##################################################
;;
;;; UI / Highlights
;;
;; ##################################################

;; *********************************
;;
;; show paren mode

;; Highlight (by bolding) the matching parenthesis

(use-package paren
  :ensure nil
  :demand t
  :defer t
  :custom-face
  (show-paren-match ((nil (:background "#9370DB" :foreground "#ffffff" :weight bold :box t)))) ;; :box t
  (show-paren-mismatch ((nil (:background "red" :foreground "black")))) ;; :box t
  :init
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis) ;; highlight brackets if visible, else entire expression
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode +1)
  )

;; *********************************
;;
;; ** Highlighting parentheses

;; This mode highlights (coloring) the current pair in which the point (cursor) is

(use-package highlight-parentheses
  :demand t
  :defer t
  :diminish
  :hook
  (prog-mode . highlight-parentheses-mode)
  :init
  (setq hl-paren-colors '("firebrick1" "IndianRed1" "IndianRed3" "IndianRed4"))
  (setq hl-paren-background-colors '("#eee222" "#ccba85" "#bceae7" "#2aa020"))
  )

;; *********************************
;;
;; ** Rainbow Delimiters

;; This highlights matching parentheses by coloring them acording to their depth
;; Specially helpful for editing lisp code


(use-package rainbow-delimiters
  :demand t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3)
  )

;; *********************************
;;
;; ** Rainbow Blocks


(use-package rainbow-blocks
  :demand t
  :defer t
  :hook
  (prog-mode . rainbow-blocks-mode)
  )


;; *********************************
;;
;; ** Highlighting numbers


(use-package highlight-numbers
  :demand t
  :defer t
  :hook
  (prog-mode . highlight-numbers-mode)
  )

;; *********************************
;;
;; ** Highlighting operators

(use-package highlight-operators
  :demand t
  :defer t
  :hook
  (prog-mode . highlight-operators-mode)
  )

;; *********************************
;;
;; ** Highlighting escape sequences


(use-package highlight-escape-sequences
  :demand t
  :defer t
  :hook
  (prog-mode . hes-mode)
  )




;; *********************************
;;
;; ** Highlight TODO

;; By default these include:
;; TODO NEXT THEM PROG OKAY DONT FAIL DONE NOTE KLUDGE HACK TEMP FIXME
;; and any sequence of X's or ?'s of length at least 3: XXX, XXXX, XXXXX, …, ???, ????, ????, ….


;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  :demand t
  :defer t
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
;;
;; ** rainbow mode

;; : Colorize hex, rgb and named color codes

(use-package rainbow-mode
  :demand t
  :defer t
  :diminish
  :hook
  (prog-mode . rainbow-mode)
  (web-mode . rainbow-mode)
  (elisp-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode)
  )


;; *********************************
;;
;; ** Highlight lines

(use-package hl-line
  :ensure nil
  :demand t ;; built-in
  :defer t
  :config
  (global-hl-line-mode)
  )


;; *********************************
;;
;; ** highlight indent guides

(use-package highlight-indent-guides
  :demand t
  :defer t
  :diminish
  :hook
  ((prog-mode yaml-mode toml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character) ; column
  )

;; ========================================
;;
;;; UI -> Modeline


;; *********************************
;; Modified or Read Only


;; This snippet displays a chain icon when the current file is saved, a broken chain when it is modified and a pad lock when the file is read only.

;; (defun custom-modeline-modified
;;   ((let* ((config-alist
;;             '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
;;               ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
;;               ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
;;            (result (cdr (assoc (format-mode-line "%*") config-alist))))
;;       (propertize (apply (cadr result) (cddr result))
;;                   'face `(:family ,(funcall (car result)))))))

;; Modeline Appearance

(set-face-attribute 'mode-line nil
  :background "#353644"
  :foreground "white"
  :box '(:line-width 5 :color "#353644")
  :overline nil
  :underline nil)

(set-face-attribute 'mode-line-inactive nil
  :background "#565063"
  :foreground "white"
  :box '(:line-width 5 :color "#565063")
  :overline nil
  :underline nil)

;; *********************************
;; Mode Icon

(defun custom-modeline-mode-icon ()
  "Display the major mode icon."
  (format " %s"
    (propertize icon
      'help-echo (format "Major-mode: `%s`" major-mode)
      'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))


;; *********************************
;;; Region Marking

(defun custom-modeline-region-info ()
  "This snippet displays useful information on the current marked region, i.e. number of lines and characters marked."
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
           (chars (count-words (region-end) (region-beginning))))
      (concat
        (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
          'face `(:family ,(all-the-icons-octicon-family))
          'display '(raise -0.0))
        (propertize (format " (%s, %s)" words chars)
          'face `(:height 0.9))))))

;; *********************************
;;; Version control icons

(defun -custom-modeline-github-vc ()
  "Custom icon for git / github repositories."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
      (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
      " · "
      (propertize (format "%s" (all-the-icons-octicon "git-branch"))
        'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
        'display '(raise -0.1))
      (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  "Custom icon for svn repositories."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
      (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
      (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  "Display a vc icon according to the type of vc system (git || svn)."
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))

;; *********************************
;;; Flycheck icons for modeline

(defun custom-modeline-flycheck-status ()
  "Display flycheck report on the modeline."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                              (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                             (+ (or .warning 0) (or .error 0)))))
                                (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                              "✔ No Issues"))
                 (`running     "⟲ Running")
                 (`no-checker  "⚠ No Checker")
                 (`not-checked "✖ Disabled")
                 (`errored     "⚠ Error")
                 (`interrupted "⛔ Interrupted")
                 (`suspicious  ""))))
    (propertize text
      'help-echo "Show Flycheck Errors"
      'mouse-face '(:box 1)
      'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

;; *********************************
;;; Time with clock icon

(defun custom-modeline-time ()
  "Display a clock icon with the current time."
  (let* ((hour (string-to-number (format-time-string "%I")))
          (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
      (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
      (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

;; *********************************
;;; LSP info in the modelin

(defun custom-lsp-mode-line ()
  "Construct the mode line text."
  (if-let (workspaces (lsp-workspaces))
    (concat "LSP" (string-join (--map (format "[%s]" (lsp--workspace-print it))
                                 workspaces)))
    (concat "LSP" (propertize "[Disconnected]" 'face 'warning))))

;; *********************************
;;
;;; My personal modeline

(setq-default mode-line-format
  (list
    ;; evil status
    evil-mode-line-tag
    '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
              'face font-lock-builtin-face))


    ;; major mode icon
    "%2 "
    "|"
    '(:eval (custom-modeline-mode-icon))

    ;; Buffer modified status
    "%2 "
    ;; '(:eval   (custom-modeline-modified))

    ;; Display a broken chain if buffer is modified
    '(:eval
       (when (eql (buffer-modified-p) t)
         ;; propertize adds metadata to text, so you can add colours and formatting, amongst other things
         (propertize (all-the-icons-faicon "chain-broken")
           'face `(:family ,(all-the-icons-faicon-family)
                    :height 1.2))))
    ;; Display a lock if buffer is readonly
    '(:eval
       (when (eql buffer-read-only t)
         (propertize (all-the-icons-faicon "lock")
           'face `(:family ,(all-the-icons-faicon-family)
                    :height 1.2))))

    ;; the buffer name; the file name as a tool tip
    ;; if buffer is modified, change the background and foreground colors
    '(:eval (propertize "%b "
              'face
              (let ((face (buffer-modified-p)))
                (if face 'font-lock-warning-face
                  'font-lock-type-face))
              'help-echo (buffer-file-name))
       (when (buffer-modified-p)
         (propertize " " 'face 'font-lock-warning-face))

       (when buffer-read-only
         (propertize "" 'face 'font-lock-warning-face))
       )

    "("
    (propertize "%02I" 'face 'font-lock-constant-face) ;; size of file
    ")"

    ;; line and column
    " "
    (propertize "%02l" 'face 'font-lock-keyword-face) ;; line number
    ":"
    (propertize "%c" 'face 'font-lock-keyword-face) ;; column number
    " "
    (propertize "%p" 'face 'font-lock-constant-face) ;; % of buffer above top of window


    ;; marked region info (lines selected, word count)
    '(:eval   (custom-modeline-region-info))


    ;; projectile
    " | "
    '(:eval (when (bound-and-true-p projectile-mode)
              (projectile-mode-line-function)))

    ;; nyan cat
    ;; "%2 "
    ;; '(:eval (when (bound-and-true-p nyan-mode)
    ;;           (nyan-create)))


    ;; party parrot
    ;; "%2 "
    ;; '(:eval (when (bound-and-true-p parrot-mode)
    ;;           (parrot-create)))

    ;; ;; spaces to align right
    ;; '(:eval (propertize
    ;;           " " 'display
    ;;           `((space :align-to (- (+ right right-fringe right-margin)
    ;;                                ,(+ 60 (string-width mode-name))))))) ;; the bigger the number, less space is added



    ;; ;; spaces to align right
    ;; '(:eval (propertize
    ;; " " 'display
    ;; `((space :align-to (- (+ right right-fringe right-margin)
    ;; 			,(+ (string-width org-mode-line-string) (+ 3 (string-width mode-name)))
    ;; 			)))))


    ;; the current major mode
    ;; '(:eval   (custom-modeline-mode-icon))
    (propertize "%m" 'face 'font-lock-string-face)

    ;; lsp status
    '(:eval (when (bound-and-true-p lsp-mode)
              " | "
              (custom-lsp-mode-line)))

    ;; day and time
    ;; '(:eval (propertize (format-time-string " %b %d %H:%M ")
    ;; 			'face 'font-lock-builtin-face))
    "|"
    '(:eval
       (custom-modeline-time))

    ;; version control
    "|"
    '(:eval
       (custom-modeline-icon-vc))
    ;; '(:eval (propertize (substring vc-mode 5)
    ;; 			'face 'font-lock-comment-face))

    ;; flycheck status
    '(:eval (when (bound-and-true-p flycheck-mode)
              "   | "
              (custom-modeline-flycheck-status)))

    )
  )


;; Diplay file size on the modeline
(size-indication-mode t)

;; ** anzu

;; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which displays current match and total matches information in the mode-line in various search modes.

(use-package anzu
  :demand t
  :defer t
  :bind
  (:map isearch-mode-map
    ([remap isearch-query-replace] . anzu-isearch-query-replace)
    ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :custom-face
  (anzu-mode-line ((nil (:foreground "yellow" :weight bold))))
  :init
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 50)
  (setq anzu-replace-to-string-separator " => ")
  :config
  (eval-when-compile
    (global-anzu-mode +1)
    (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
  )

;; Minions

;; Group all minor modes in a single menu in the modeline

(use-package minions
  :demand t
  :config
  (minions-mode 1)
  )

;; *********************************
;;
;; parrot-mode

(use-package parrot
  :demand t
  ;; :hook
  ;; (parrot-click . parrot-start-animation)
  ;; (after-save . parrot-start-animation)
  ;; (self-insert . parrot-start-animation)
  ;; (parrot-click-hook . flyspell-buffer)
  :config
  ;; To see the party parrot in the modeline, turn on parrot mode:
  (parrot-mode)
  (parrot-set-parrot-type 'default)
  ;; (setq parrot-num-rotations nil) ;; new! make parrot rotate forever
  ;; (setq parrot-animate-parrot t) ;; suposedly animates parrot, same as variable above
  ;;/Rotation function keybindings for evil users
  (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
  (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
  )


;; *********************************
;;
;; nyan-mode

(use-package nyan-mode
  :demand t
  ;; :if window-system
  :init
  (setq nyan-cat-face-number 4)
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  :config
  (nyan-mode)
  (nyan-start-animation)
  )

;; *********************************
;;
;; Post Initialization

;; Let's lower our GC thresholds back down to a sane level.

(setq gc-cons-threshold 16777216
gc-cons-percentage 0.1)
