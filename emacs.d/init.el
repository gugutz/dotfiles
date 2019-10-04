(defun /util/tangle-init ()
  (interactive)
  "If the current buffer is init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "emacs.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook #'/util/tangle-init)

;; (use-package server
;;   :ensure nil
;;   :init
;;   (unless (or (daemonp) (server-running-p))
;;     (server-start))
;;   :hook (after-init . server-mode))

(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

(setq create-lockfiles nil)

(setq x-select-enable-clipboard t)

(setq vc-follow-symlinks t)

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

(setq ring-bell-function 'ignore)

(setq initial-scratch-message nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

; parentheses
(show-paren-mode t)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq linum-format " %d ")

;; (global-subword-mode 1)

(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

(setq next-line-add-newlines t)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'prog-mode-hook 'remove-dos-eol)

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-_") #'text-scale-decrease)
;; (global-set-key (kbd "C-)") #'text-scale-adjust)

(global-set-key "\M- " 'hippie-expand)

(defun upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg)))

(defun downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg)))

(defun capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg)))

(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
;; this replaces native capitlize word!
(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defmacro /bindings/define-prefix-keys (keymap prefix &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(caddr binding)))
                   (define-key ,keymap (kbd seq) func)
                   (when desc
                     (which-key-add-key-based-replacements
                       (if ,prefix
                           (concat ,prefix " " seq)
                         seq)
                       desc))))))

(defmacro /bindings/define-keys (keymap &rest body)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil ,@body))

(defmacro /bindings/define-key (keymap sequence binding &optional description)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil
     (,sequence ,binding ,description)))

;; examples
;; after [evil magit] (
  ;; execute after evil and magit have been loaded
;  )

;; macro definiton
(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.

FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(defun my-save ()
  "Save file when leaving insert mode in Evil."
  (if (buffer-file-name)
      (evil-save)))

;; (add-hook 'evil-insert-state-exit-hook 'my-save)

(require 'epa-file)
(epa-file-enable)

(require 'package)
;; add melpa stable emacs package repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   (quote
   (pdf-tools ox-pandoc ox-reveal org-preview-html latex-preview-pane smart-mode-line-powerline-theme base16-theme gruvbox-theme darktooth-theme rainbow-mode smartscan restclient editorconfig prettier-js pandoc rjsx-mode js2-refactor web-mode evil-org multiple-cursors flycheck smart-mode-line ## evil-leader evil-commentary evil-surround htmlize magit neotree evil json-mode web-serverx org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
;; (add-to-list 'load-path "~/dotfiles/emacs.d/config")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'evil.tau)
(require 'org.tau)
(require 'ruby.tau)
(require 'elixir.tau)

(when (eq window-system nil)
  (xterm-mouse-mode 1))

;; (use-package mouse3
;;     :config
;; (global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu))

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
  (right-click-context-menu))))

;; for some readon the bellow lines should be the default native way for navigation on emacs
;; but they dont work
;; using the above package instead til i find a solution
;
(windmove-default-keybindings 'control)
(global-set-key (kbd "C-S-H") 'windmove-left)
(global-set-key (kbd "C-S-L") 'windmove-right)
(global-set-key (kbd "C-S-K") 'windmove-up)
(global-set-key (kbd "C-S-J") 'windmove-down)

;; (require 'evil-tmux-navigator)

(define-prefix-command 'evil-window-map)
(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "j" 'evil-window-down)
(define-key evil-window-map "k" 'evil-window-up)
(define-key evil-window-map "l" 'evil-window-right)
(define-key evil-window-map "b" 'evil-window-bottom-right)
(define-key evil-window-map "c" 'evil-window-delete)
(define-key evil-motion-state-map "\M-w" 'evil-window-map)

;; (/bindings/define-keys evil-normal-state-map
  ;; ("C-w h" #'evil-window-left)
  ;; ("C-w j" #'evil-window-down)
  ;; ("C-w k" #'evil-window-up)
  ;; ("C-w l" #'evil-window-right))

;; (/bindings/define-keys evil-normal-state-map
;;   ("C-w h" #'evil-window-left)
;;   ("C-w j" #'evil-window-down)
;;   ("C-w k" #'evil-window-up)
;;   ("C-w l" #'evil-window-right))

(use-package multiple-cursors
  ;; step 1, select thing in visual-mode (OPTIONAL)
  ;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
  ;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
  ;; step 4, press s or S to start replace
  ;; step 5, press C-g to quit multiple-cursors
  :bind
  ("M-u" . hydra-multiple-cursors/body)
  :config
  (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor))

(use-package helm
  :ensure t
  :defer t
  :bind
  ("M-x" . helm-M-x)
  :config
  (progn
    (helm-mode 1))
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-bookmark-show-location t)
  (setq helm-buffer-max-length 40)
  (setq helm-split-window-inside-p t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-follow-mode-persistent t)

  ;; take between 10-30% of screen space
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 30)
  (helm-autoresize-mode t)
  ;; Make helm replace the default Find-File and M-x
  (progn
    (global-set-key [remap execute-extended-command] #'helm-M-x)
    (global-set-key [remap find-file] #'helm-find-files)
    (helm-mode t)))
  (require 'helm-config)
  (global-set-key (kbd "C-c h") #'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; (global-set-key (kbd "C-h a") #'helm-apropos)
  (global-set-key (kbd "C-x b") #'helm-buffers-list)
  (global-set-key (kbd "C-x C-b") #'helm-mini)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x r b") #'helm-bookmarks)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "M-:") #'helm-eval-expression-with-eldoc)
  ;; (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  ;; (define-key helm-map (kbd "C-z") #'helm-select-action)

(use-package projectile
  :config
  ;; (projectile-mode)
  (projectile-mode +1)
  (setq projectile-globally-ignored-files
        (append '("~"
                  ".swp"
                  ".pyc")
                projectile-globally-ignored-files))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(after 'dired
  (require 'dired-k)
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (add-hook 'dired-initial-position-hook #'dired-k))

(setq dired-dwin-target t)

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
      ))

; Pull system shell in a new bottom window
(define-key evil-normal-state-map (kbd "\"") #'/shell/new-window)
(define-key evil-visual-state-map (kbd "\"") #'/shell/new-window)
(define-key evil-motion-state-map (kbd "\"") #'/shell/new-window)

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

;; (pdf-tools-install)
;; the docs say if i care about startup time, i should use pdf-loader-install instead of pdf-tools-install, but doenst say why
;; (pdf-loader-install)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)

(evil-define-key 'normal pdf-view-mode-map
  "h" 'pdf-view-previous-page-command
  "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
  "l" 'pdf-view-next-page-command)

(setq inhibit-splash-screen t)

(blink-cursor-mode t)
(setq blink-cursor-blinks 0) ;; blink forever
(setq-default indicate-empty-lines t)
(setq-default line-spacing 3)
(setq frame-title-format '("Emacs"))

(scroll-bar-mode -1)

(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")
; theme options:
; atom-one-dark (doenst work well with emacsclient, ugly blue bg)
; dracula
; darktooth
; gruvbox-dark-hard
; gruvbox-dark-light
; gruvbox-dark-medium
; base16-default-dark-theme -- this one is good

(setq my-theme 'darkplus)

(load-theme my-theme t)

;; (defun load-my-theme (frame)
;;   "Function to load the theme in current FRAME.
;;   sed in conjunction
;;   with bellow snippet to load theme after the frame is loaded
;;   to avoid terminal breaking theme."
;;   (select-frame frame)
;;   (load-theme my-theme t))

;; ; make emacs load the theme after loading the frame
;; ; resolves issue with the theme not loading properly in terminal mode on emacsclient

;; ;; this if was breaking my emacs!!!!!
;;  (add-hook 'after-make-frame-functions #'load-my-theme)

(require 'doom-modeline)
(doom-modeline-mode 1)

;; (setq inhibit-compacting-font-caches t)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 23)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project = ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project = ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project = emacs/l/comint.el
;;   truncate-except-project = ~/P/F/emacs/l/comint.el
;;   truncate-upto-root = ~/P/F/e/lisp/comint.el
;;   truncate-all = ~/P/F/e/l/comint.el
;;   relative-from-project = emacs/lisp/comint.el
;;   relative-to-project = lisp/comint.el
;;   file-name = comint.el
;;   buffer-name = comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setq doom-modeline-icon t)

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Whether display buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
(setq doom-modeline-persp-name-icon nil)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
;; (setq doom-modeline-env-enable-python t)
;; (setq doom-modeline-env-enable-ruby t)
;; (setq doom-modeline-env-enable-perl t)
;; (setq doom-modeline-env-enable-go t)
;; (setq doom-modeline-env-enable-elixir t)
;; (setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

(require 'parrot)
;; To see the party parrot in the modeline, turn on parrot mode:
(parrot-mode)

(global-set-key (kbd "C-c p") 'parrot-rotate-prev-word-at-point)
(global-set-key (kbd "C-c n") 'parrot-rotate-next-word-at-point)

(define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
(define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)

(parrot-set-parrot-type 'default)

; parrot-animation-frame-interval

; parrot-minimum-window-width

; (parrot-animate-parrot t)

; parrot-spaces-before
; parrot-spaces-after

; - number of times the parrot will cycle through its gif.
parrot-num-rotations

(add-hook 'mu4e-index-updated-hook #'parrot-start-animation)

(add-hook 'parrot-click-hook #'parrot-start-animation)

(add-hook 'after-save-hook #'parrot-start-animation)

(use-package nyan-mode
   :if window-system
   :hook
   (after-init . nyan-mode)
   :config
   (setq nyan-cat-face-number 4)
   (setq nyan-animate-nyancat t)
   (setq nyan-wavy-trail t)
   (nyan-start-animation))

;; (use-package solaire-mode
;;   :config
;;   (solaire-mode)
;;   :hook
;;   (after-init . solaire-global-mode +1)
;;   ;; To enable solaire-mode unconditionally for certain modes:
;;   (ediff-prepare-buffer . solaire-mode)
;;   ;; if you use auto-revert-mode, this prevents solaire-mode from turning itself off every time Emacs reverts the file
;;   (after-revert- . turn-on-solaire-mode)
;;   ;; highlight the minibuffer when it is activated:
;;   (minibuffer-setup . solaire-mode-in-minibuffer)
;;   (after-change-major-mode . turn-on-solaire-mode)
;;   :config
;;   ;; if the bright and dark background colors are the wrong way around, use this
;;   ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;;   ;; This should be used *after* you load the active theme!
;;   ;;  NOTE: This is necessary for themes in the doom-themes package!
;;   (solaire-mode-swap-bg))

(require 'centaur-tabs)
(centaur-tabs-mode t)

(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(global-set-key [?\C-.] 'centaur-tabs-forward-tab)
(global-set-key (kbd "C-,") 'centaur-tabs-backward-tab)

(setq centaur-tabs-style "bar")

(setq centaur-tabs-height 32)

(setq centaur-tabs-set-icons t)

(setq centaur-tabs-set-bar 'over)

(setq centaur-tabs-gray-out-icons 'buffer)

(setq centaur-tabs-set-modified-marker t)

(setq centaur-tabs-modified-marker "*")

(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package highlight-operators
  :ensure t
  :hook ((prog-mode . highlight-operators-mode)))

(use-package highlight-escape-sequences
  :ensure t
  :hook ((prog-mode . hes-mode)))

(use-package highlight-parentheses
  :ensure t
  :hook ((prog-mode . highlight-parentheses-mode)))

;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'highlight-operators-mode)
;; (add-hook 'prog-mode-hook 'hes-mode)    ;; highlight escape sequences

(use-package which-key
  :hook (after-init . which-key-mode))
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-min-display-lines 3)
  (setq which-key-max-description-length 20)
  (setq which-key-max-display-columns 6)

(global-diff-hl-mode)

(use-package smartparens
  :ensure t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package evil-smartparens
  :ensure t
  :hook
  (smarparens-enabled . evil-smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook
  (org-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  (php-mode . rainbow-mode)
  (html-mode . rainbow-mode)
  (web-mode . rainbow-mode)
  (js2-mode . rainbow-mode))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
    (setq emmet-indentation 2)
    (setq emmet-move-cursor-between-quotes t)
  :hook
    (sgml-mode . emmet-mode) ;; Auto-start on any markup modes
    (css-mode . emmet-mode) ;; enable Emmet's css abbreviation.
    (html-mode . emmet-mode) ;; Auto-start on HTML files
    (web-mode . emmet-mode) ;; Auto-start on web-mode
  :config
    (setq emmet-expand-jsx-className? t)) ;; use emmet with JSX markup

(smartscan-mode 1)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package flycheck
    :ensure t
    :defer t
    :hook
    (after-init . global-flycheck-mode)
    :init
    (global-flycheck-mode))

(with-eval-after-load 'flycheck
  (global-flycheck-inline-mode))
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))

(require 'web-mode)

(setq web-mode-code-indent-offset 2)

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.ts$" . js2-mode))
  :hook ((js2-mode . flycheck-mode)
         (js2-mode . company-mode)
         (js2-mode . lsp-mode)
         (js2-mode . add-node-modules-path))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)

  ;; use eslint_d insetad of eslint for faster linting
  ;; (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil))

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--bracket-spacing" "false"))
  (defun enable-minor-mode (my-pair)

  "Enable prettier-js-mode if theres a .prettierrc on project dir"
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
      ; Hook the above function to web-mode
  (add-hook 'web-mode-hook #'(lambda ()
      (enable-minor-mode
          '("\\.js?\\'" . prettier-js-mode)
          '("\\.jsx?\\'" . prettier-js-mode)
          '("\\.css?\\'" . prettier-js-mode)))))

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2))

;; eslintd-fix: Emacs minor-mode to automatically fix javascript with eslint_d.
;; https://github.com/aaronjensen/eslintd-fix/tree/master
;; (use-package eslintd-fix)

(use-package rjsx-mode
    :after js2-mode
    :mode
    (("\\.jsx$" . rjsx-mode)
    ("components/.+\\.js$" . rjsx-mode)))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil)
  :hook ((enh-ruby-mode . lsp)
         (js2-mode . lsp)
         (js2-jsx-mode . lsp)))

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(use-package lsp-ui
  :ensure t
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  (setq ;; lsp-ui-doc
        lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'top ;; top, bottom, or at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 30
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit t
        ;; lsp-ui-flycheck
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        ;; lsp-ui-sideline
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-code-actions-prefix ""
        lsp-ui-sideline-update-mode 'point
        ;; lsp-ui-imenu
        lsp-ui-imenu-enable t
        lsp-ui-imenu-kind-position 'top
        ;; lsp-ui-peek
        lsp-ui-peek-enable t
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-list-width 50
        lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  :bind
    (:map lsp-mode-map
      ("C-c C-r" . lsp-ui-peek-find-references)
      ("C-c C-j" . lsp-ui-peek-find-definitions)
      ("C-c i"   . lsp-ui-peek-find-implementation)
      ("C-c m"   . lsp-ui-imenu)
      ("C-c s"   . lsp-ui-sideline-mode)
      ("C-c d"   . ladicle/toggle-lsp-ui-doc)))

;; (define-key ac-completing-map [return] nil)
;; (define-key ac-completing-map "\r" nil)

(require 'auto-complete)
(global-auto-complete-mode t)

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (define-key evil-insert-state-map (kbd "TAB")
       #'company-indent-or-complete-common)
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-minimum-prefix-length 1)               ; start completing after 1st char typed
  (setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (setq company-dabbrev-downcase nil)                  ; Do not convert to lowercase
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-everywhere t)
  (setq company-selection-wrap-around t)               ; continue from top when reaching bottom
  (setq company-auto-complete 'company-explicit-action-p)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  ;; (setq company-tooltip-flip-when-above t)
  (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil)))

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package yasnippet                  ; Snippets
  :ensure t
  :config
    (setq yas-verbosity 1)                      ; No need to be so verbose
    (setq yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  ;; Bind `SPC' to `yas-expand' when snippet expansion available (it
  ;; will still call `self-insert-command' otherwise).
  ;; yas-maybe-expand contains a special value which, when bound in a keymap,
  ;; tells Emacs to call yas-expand if and only if there is a snippet abbrev before point.
  ;; If there is no snippet to expand, Emacs will behave as if yas-expand is unbound
  ;; and so will run whatever command is bound to that key normally.
  (define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)
  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

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

(evil-define-command paste-from-clipboard()
  (if (display-graphic-p)
      (progn
	(clipboard-yank)
	(message "graphics active")
	)
    (insert (shell-command-to-string "xsel -o -b")) ) )

(global-set-key [f9] 'copy-to-clipboard)
(global-set-key [f10] 'paste-from-clipboard)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:


(provide 'init)
;;; .emacs ends here
