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

(defvar tau/erc-nick               nil        "The ERC nick to use.")
(defvar tau/erc-password           nil        "The ERC password to use.")
(defvar tau/erc-port               nil        "The ERC port to use.")
(defvar tau/erc-server             nil        "The ERC server to use.")
(defvar tau/font-family            "Courier"  "The font to use.")
(defvar tau/font-size-default      110        "The font size to use for default text.")
(defvar tau/font-size-header-line  80        "The font size to use for the header-line.")
(defvar tau/font-size-mode-line    100        "The font size to use for the mode-line.")
(defvar tau/font-size-small        100        "The font size to use for smaller text.")
(defvar tau/font-size-title        140        "The font size to use for titles.")

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

;; First save the current value of gc-cons-threshold to restore it after the init file is loaded at the very bottom of this file
(setq gc-threshold-original gc-cons-threshold)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000) ;; 50mb
;;(setq gc-cons-threshold 100000000) ;; 100mb

;; GC only with 500mb of data allocated
;; (setq gc-cons-percentage 0.5)

;; GC after 5s idle time
;; (run-with-idle-timer 5 t #'garbage-collect)

(setq garbage-collection-messages t)
(setq inhibit-compacting-font-caches t)      ;; Don’t compact font caches during GC (garbage collection).

;; Restore original gc value after init
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold gc-threshold-original)))

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

;; Init time start
(defvar my-init-el-start-time (current-time) "Time when init.el was started")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t
  :init
  ;; use sudo when needed
  (setq system-packages-use-sudo t)
)

(require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7) ;; in days
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
)

(use-package diminish
  :ensure t
)

(use-package epa-file
  :config
  (epa-file-enable)
  (setq epa-file-encrypt-to '("gugutz@gmail.com"))

  ;; Control whether or not to pop up the key selection dialog.
  (setq epa-file-select-keys 0)
  ;; Cache passphrase for symmetric encryption.
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
)

(use-package gnus
  :ensure nil
  :config
  (setq user-mail-address "joshuafwolfe@gmail.com"
        user-full-name "Josh Wolfe")

  (setq gnus-select-method
        '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl)))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))

  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5))

  (define-key gnus-group-mode-map
    ;; list all the subscribed groups even they contain zero un-read messages
        (kbd "o") 'my-gnus-group-list-subscribed-groups)
)

;; (use-package server
;;   :ensure nil
;;   :init
;;   (unless (or (daemonp) (server-running-p))
;;     (server-start))
;;   :hook (after-init . server-mode))

(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

(require 'cl)
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

;; define list of fonts to be used in the above function
;; the first one found will be used
(set-face-attribute 'default nil :font (font-candidate '"DejaVu Sans Mono-10:weight=normal"
                                                        "Hack-10:weight=normal"
                                                        "Consolas-10:weight=normal"
                                                        "Droid Sans Mono-10:weight=normal"
                                                        "DejaVu Sans Mono-10:weight=normal"
                                                        "Ubuntu Mono-12:weight=normal"))

(use-package visual-line-mode
  :ensure nil
  :hook
  (prog-mode . turn-on-visual-line-mode)
  (text-mode . turn-on-visual-line-mode)
)

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

(use-package files
  :ensure nil
  :config
  (setq make-backup-files nil)
  ;; dont ask confirmation to kill processes
  ;;(setq confirm-kill-processes nil)
)

(setq confirm-kill-processes nil)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

(setq large-file-warning-threshold nil) ;; Don’t warn me about opening large files

(use-package display-line-numbers
  :if (version<= "26.0.50" emacs-version)
  :ensure nil
  :init
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  ;; old linum-mode variables, check if they work with new display-line-numbers-mode
  ;; (setq linum-format 'dynamic)
  ;; (setq linum-format " %d ") ;; one space separation between the linenumber display and the buffer contents:
  ;; (setq linum-format "%4d “) ;; 4 character and a space for line numbers
  (setq linum-format "%4d \u2502 ") ; 4 chars and a space with solid line separator
  :config
  ;;(global-display-line-numbers-mode)
  ;; for some reason the hooks for diplay line numbers wont work if i put them in use-package `:hook'. it has to be after `:config'
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

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

(savehist-mode 1)

(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 0.5)
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-verbose nil)
)

(setq next-line-add-newlines t)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
)

(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'prog-mode-hook 'remove-dos-eol)

(global-set-key (kbd "C-S-+") #'text-scale-increase)
(global-set-key (kbd "C-S-_") #'text-scale-decrease)
(global-set-key (kbd "C-S-)") #'text-scale-adjust)

(use-package expand-region
  :ensure nil
  :bind
  ([(control shift iso-lefttab)] . 'er/expand-region)
)

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; Kill current buffer; prompt only if
;; there are unsaved changes.
(global-set-key (kbd "C-x k")
  '(lambda () (interactive) (kill-buffer (current-buffer)))
)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq require-final-newline t)

(setq-default fill-column 80)
(setq auto-fill-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(delete-selection-mode 1)

(setq window-combination-resize t)

;; (setq-default line-spacing 1) ;; A nice line height
(setq-default line-spacing 3)

(setq system-uses-terminfo nil) ;; Fix weird color escape sequences

;; (setq confirm-kill-emacs 'yes-or-no-p) ;; Ask for confirmation before closing emacs

(setq help-window-select t)

(use-package subword
  :ensure nil
  :hook
  (clojure-mode . subword-mode)
  (ruby-mode . subword-mode)
  (enh-ruby-mode . subword-mode)
  (elixir-mode . subword-mode)
)

(use-package superword
  :ensure nil
  :hook
  (js2-mode . superword-mode)
)

(setq-default indent-tabs-mode nil)
;; C e C-like langs default indent size
(setq-default tab-width 2)
;; Perl default indent size
(setq-default cperl-basic-offset 2)
(setq-default c-basic-offset 2)

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

(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode)
)

(use-package eldoc
  :ensure nil
  :hook
  (prog-mode . eldoc-mode)
  ;;(prog-mode       . turn-on-eldoc-mode)
  ;; (cider-repl-mode . turn-on-eldoc-mode)
  :config
  ;; (global-eldoc-mode -1)
  ;; (add-hook 'prog-mode-hook 'eldoc-mode)
  (setq eldoc-idle-delay 0.4)
)

(use-package eldoc-box
  :ensure t
  :after eldoc
  :custom-face
  ;;(eldoc-box-border (t (:background "#202020"))))
  ;;(eldoc-box-body (t (:background "#202020"))))
  :config
  ;;(setq eldoc-box-max-pixel-width)
  ;;(setq eldoc-box-max-pixel-height)
  ;;(setq eldoc-box-only-multi-line)   ;;  Set this to non-nil and eldoc-box only display multi-line message in childframe. One line messages are left in minibuffer.
  ;; (eldoc-box-hover-mode)
  (eldoc-box-hover-at-point-mode)
)

(use-package aggressive-indent
  :ensure t
  :custom
  (aggressive-indent-comments-too t)
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  :config
)

(use-package ialign
  :ensure t
  :bind
  ("C-x l" . ialign)
  :config
  ;;(setq ialign-default-spacing 32)
  (setq ialign-align-with-tabs nil) ;; default nil
  (setq ialign-auto-update t) ;; default t
)

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

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(use-package dumb-jump
  :ensure t
  :after helm
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (eval-when-compile
    (require 'helm-source nil t))
  (setq dumb-jump-selector 'helm)
  ;; (setq dumb-jump-selector 'ivy)
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg))
)

(defun downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg))
)

(defun capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg))
)

(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
(global-set-key (kbd "C-M-l")	 'downcase-backward-WORD)
;; this replaces native capitlize word!
(global-set-key (kbd "C-M-c")	 'capitalize-backward-word)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

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
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

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
  (right-click-context-menu)))
)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

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
          indent-according-to-mode
          emmet-expand-line
          company-indent-or-complete-common
          ))
)

(use-package evil
  :ensure t
  :init
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-esc-delay 0)  ;; Don't wait for any other keys after escape is pressed.
  ;; Make Evil look a bit more like (n) vim  (??)
  (setq evil-search-module 'isearch-regexp)
  ;; (setq evil-search-module 'evil-search)
  (setq evil-magic 'very-magic)
  (setq evil-shift-width (symbol-value 'tab-width))
  (setq evil-regexp-search t)
  (setq evil-search-wrap t)
  ;; (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo nil)
  (setq evil-want-integration nil)
  ;; (setq evil-want-abbrev-on-insert-exit nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-mode-line-format '(before . mode-line-front-space)) ;; move evil tag to beginning of modeline
  ;; Cursor is alway black because of evil.
  ;; Here is the workaround
  ;; (@see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default)
  (setq evil-default-cursor t)
  ;; change cursor color according to mode
  (setq evil-emacs-state-cursor '("#ff0000" box))
  (setq evil-motion-state-cursor '("#FFFFFF" box))
  (setq evil-normal-state-cursor '("#00ff00" box))
  (setq evil-visual-state-cursor '("#abcdef" box))
  (setq evil-insert-state-cursor '("#e2f00f" bar))
  (setq evil-replace-state-cursor '("red" hbar))
  (setq evil-operator-state-cursor '("red" hollow))

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

(use-package evil-org
  :after org evil
  :hook
  (org-mode . evil-org-mode)
  :config
  (lambda ()
    (evil-org-set-key-theme))
)

(use-package evil-numbers
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
  ("C-c C-+" . evil-numbers/inc-at-pt)
  ("C-c C--" . evil-numbers/dec-at-pt)
  ("<kp-add>" . evil-numbers/inc-at-pt)
  ("<kp-subtract>" . evil-numbers/dec-at-pt))
  :config
  (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt)
)

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
    "b" 'switch-to-buffer
    "-" 'split-window-bellow
    "|" 'split-window-right
    "." 'find-tag
    "t" 'projectile-find-file
    "b" 'ido-switch-buffer
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
    "x" 'smex)
    "|" 'split-window-right
)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

(defun evil-surround-prog-mode-hook-setup ()
  "Documentation string, idk, put something here later."
  (push '(47 . ("/" . "/")) evil-surround-pairs-alist)
  (push '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(41 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
)
(add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)

(defun evil-surround-js-mode-hook-setup ()
  "ES6." ;  this is a documentation string, a feature in Lisp
  ;; I believe this is for auto closing pairs
  (push '(?1 . ("{`" . "`}")) evil-surround-pairs-alist)
  (push '(?2 . ("${" . "}")) evil-surround-pairs-alist)
  (push '(?4 . ("(e) => " . "(e)")) evil-surround-pairs-alist)
  ;; ReactJS
  (push '(?3 . ("classNames(" . ")")) evil-surround-pairs-alist)
)
(add-hook 'js2-mode-hook 'evil-surround-js-mode-hook-setup)

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)
)
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

(defun evil-surround-org-mode-hook-setup ()
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
)
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)

(use-package evil-commentary
  :config
  (evil-commentary-mode)
)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1)
)

(use-package evil-paredit
  :ensure t
  :hook
  (emacs-lisp-mode . evil-paredit-mode)
)

(use-package evil-mc
  :ensure t
  :after evil
  :bind
  (:map evil-visual-state-map
  ("C-d" . evil-mc-make-and-goto-next-match) ;; Make a cursor at point and go to the next match of the selected region or the symbol under cursor.
  ("C-a" . evil-mc-make-all-cursors) ;; Create cursors for all strings that match the selected region or the symbol under cursor.
  ("C-q" . evil-mc-undo-all-cursors)  ;; Remove all cursors.
  )
  :config
  (global-evil-mc-mode  1)
)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t) ;; default is to pulse when running in a graphic display
  (setq evil-goggles-duration 0.200) ;; default is 0.200

;; list of all on/off variables, their default value is `t`:

  (setq evil-goggles-enable-paste nil) ;; to disable the hint when pasting
;;(setq  evil-goggles-enable-delete t)
;;(setq  evil-goggles-enable-change t)
;;(setq evil-goggles-enable-indent t)
;;(setq  evil-goggles-enable-yank t)
;;(setq  evil-goggles-enable-join t)
;;(setq evil-goggles-enable-fill-and-move t)
;;(setq evil-goggles-enable-paste t)
;;(setq evil-goggles-enable-shift t)
;;(setq evil-goggles-enable-surround t)
;;(setq evil-goggles-enable-commentary)
;;(setq evil-goggles-enable-nerd-commenter t)
;;(setq evil-goggles-enable-replace-with-register t)
;;(setq evil-goggles-enable-set-marker t)
;;(setq evil-goggles-enable-undo t)
;;(setq evil-goggles-enable-redo t)
;;(setq evil-goggles-enable-record-macro t)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
)

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

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :defer t
  :hook
  (org-mode . diff-hl-mode)
  (org-mode . rainbow-mode)
  (org-mode . turn-on-visual-line-mode)
  (org-mode . color-identifier-mode)
  (org-mode . org-bullets-mode)
  (org-mode . flycheck-mode)
  (org-mode . company-mode)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)

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

(use-package org-sidebar
:ensure t
:after org
:bind
("S-<f8>" . org-sidebar-tree-toggle)
)

(use-package ox-extra
  :ensure nil
  :config
  (ox-extras-activate '(ignore-headlines))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
)

(require 'org-habit nil t)

(defun org-add-my-extra-fonts ()
  "Add alert and overdue fonts."
  (add-to-list 'org-font-lock-extra-keywords '("\\(!\\)\\([^\n\r\t]+\\)\\(!\\)" (1 '(face org-habit-alert-face invisible t)) (2 'org-habit-alert-face) (3 '(face org-habit-alert-face invisible t))))
  (add-to-list 'org-font-lock-extra-keywords '("\\(%\\)\\([^\n\r\t]+\\)\\(%\\)" (1 '(face org-habit-overdue-face invisible t)) (2 'org-habit-overdue-face) (3 '(face org-habit-overdue-face invisible t)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)

(use-package ox-latex
  :after org
  :ensure nil
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

(use-package ox-pandoc
  :after (org ox)
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

(use-package org-bullets
  :ensure t
  :config
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◉" "○" "●" "►" "•"))
)

(use-package ox-confluence
  :defer 3
  :ensure nil
  :after org
)

(use-package ox-reveal
  :ensure t
  :after ox
  :config
  ;;(setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
)

(use-package ox-md
  :defer t
  :after org
)

(use-package ox-gfm
  :ensure t
  :defer t
  :after org
)

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
)

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
(projectile-completion-system 'helm)
  :config
  (projectile-mode +1)
  (setq projectile-globally-ignored-files
        (append '("~"
                  ".swp"
                  ".pyc")
                projectile-globally-ignored-files))
)

(use-package helm-projectile
  :ensure t
;  :after projectile
;  :demand t
  :config
  (helm-projectile-on)
)

(use-package org-kanban
    :ensure t
  :after org
:commands  (org-kanban/initialize)
    :config
    )

(use-package org-jira
  :ensure t
  :disabled
  :defer 3
  :commands (org-jira-mode org-jira-get-issues org-jira-get-projects)
  :after org
  :custom
  (jiralib-url "https://stairscreativestudio.atlassian.net")
  :config
  (setq jiralib-token
  `("Cookie" . ,(format "ajs_group_id=null; ajs_anonymous_id=%222e15ea7d-cb97-4d24-bfe2-348c4655df02%22; atlassian.xsrf.token=86ec43db-1a9e-48fa-892a-9210c6fee684_38750fda3fb7e52a86533ed838cb5688d745af60_lin; cloud.session.token=eyJraWQiOiJzZXNzaW9uLXNlcnZpY2VcL3Nlc3Npb24tc2VydmljZSIsImFsZyI6IlJTMjU2In0.eyJhc3NvY2lhdGlvbnMiOlt7ImFhSWQiOiI1YmE3ZjkyNDNjMDEzZDdiMTA1MTRjYmIiLCJzZXNzaW9uSWQiOiJhYWUwYmVkNC02MTY1LTQ4MjUtYThkYS03ZWEwNGIxMWQ3MzgiLCJlbWFpbCI6Imd1Z3V0ekBnbWFpbC5jb20ifV0sInN1YiI6IjVkOTM1MzI1MGMyYTVkMGRkODdhZmQ2MCIsImVtYWlsRG9tYWluIjoic3RhaXJzLnN0dWRpbyIsImltcGVyc29uYXRpb24iOltdLCJyZWZyZXNoVGltZW91dCI6MTU3MjYzODk5NywidmVyaWZpZWQiOnRydWUsImlzcyI6InNlc3Npb24tc2VydmljZSIsInNlc3Npb25JZCI6ImYwNTEwYzA5LWRmNGMtNGE2MC1iYTM4LTA2OTU2YzRiODRkMSIsImF1ZCI6ImF0bGFzc2lhbiIsIm5iZiI6MTU3MjYzODM5NywiZXhwIjoxNTc1MjMwMzk3LCJpYXQiOjE1NzI2MzgzOTcsImVtYWlsIjoiZ3VzdGF2b0BzdGFpcnMuc3R1ZGlvIiwianRpIjoiZjA1MTBjMDktZGY0Yy00YTYwLWJhMzgtMDY5NTZjNGI4NGQxIn0.naz3Vi6yvn0aoFX7nAMK-K7fff4zpkeUifPSrEj6a3so9pK6uPrMDZOIGd8Mg7pJJkCy8FJ9bC6eTCGdrbqll3v8Kg6NhThAQzx8tvcW4gFObJyL12HEvt9EBpwvGKW1mWLhb-S_ZGwoTCXk1QRpNHy6zNl3etwlhX9jk3KXXT5fIaO2oJJaFCovZRvQTJdyoCRiIBRPWwyh3tqrqJiZVD08NFY1bq_aCyfkxxN-owWP7KPJxmLtH-ZPpj24ky8Dv-4oVP_frUPkLW5ULvHstdhxkwCUWCpaTPPDBqijljTj5YvXFp_ulNyWqQHnPHeV3m6BszI9WxBxZF7mUwIBZw; _csrf=AMoq40Bo50JzeFptjXhUvsBl")))
  (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
  (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
  (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
  (define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
  (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
  (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
  (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
  (define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
  (define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
  (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
  (define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
  (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
  (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
  (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
  (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
  (define-key org-jira-map (kbd "C-c cc") 'org-jira-add-comment)
  (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
  (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
  (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
  (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)
)

(defun tau/hydra-jira ()
  (interactive)
  (funcall
      (pretty-hydra-define hydra-jira (:exit t :hint nil)
        ("Get" (("p" org-jira-get-projects                "Get Projects")
                ("g" org-jira-get-issues                  "Get Issues")
                ("G" org-jira-get-subtasks                "Get Subtasks")
                ("r" org-jira-refresh-issue               "Refresh Issue")
                ("R" org-jira-refresh-issues-in-buffer    "Refresh Issues in Buffer"))

       "Manage" (("b" org-jira-browse-issue             "Browse Issue")
                 ("c" org-jira-create-issue             "Create Issue")
                 ("s" org-jira-create-subtask           "Create Subtask")
                 ("P" org-jira-progress-issue           "Update Issue Progress")
                 ("a" org-jira-assign-issue             "Assign Issue"))

         "Push" (("u" org-jira-update-issue                "Update Issue")
                 ("y" org-jira-copy-current-issue-key      "Copy Current Issue Key")
                 ("U" org-jira-update-comment              "Update Comment")
                 ("t" org-jira-todo-to-jira                "Todo to Jira")))))
)

(use-package ox-jira
  :defer 3
  :after org
)

(use-package ejira
  :ensure t
  :disabled
  :init
  (setq jiralib2-url              "https://stairscreativestudio.atlassian.net"
        jiralib2-auth             'basic
        jiralib2-user-login-name  "gustavo@stairs.studio"
        jiralib2-token            nil

        ejira-org-directory       "~/jira"
        ejira-projects            '("PEP" "QuexCash")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me"))))))
)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook
  (after-init . ivy-mode)
  :custom
  (ivy-re-builders-alist
  '((t . ivy--regex-plus)))
  :config
  (ivy-mode)
  ;; display an arrow on the selected item in the list
  (setf (cdr (assoc t ivy-format-functions-alist)) #'ivy-format-function-arrow)

  (setq ivy-display-style 'fancy
     ivy-use-virtual-buffers t
     enable-recursive-minibuffers t
     ivy-use-selectable-prompt t)
  (ivy-set-actions  t
  '(("I" insert "insert")))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
)

(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  :defines
  (projectile-completion-system magit-completing-read-function)
  :hook
  (ivy-mode . counsel-mode)
  :custom
  (counsel-yank-pop-height 15)
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-on-del-error-function nil)
  (swiper-action-recenter t)
  (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
  ;; check out this better-jumper mode to see what it does
  ;; (counsel-grep-post-action . better-jumper-set-jump)
  :preface
  (defun ivy-format-function-pretty (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat
           (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
           (ivy--add-face str 'ivy-current-match)))

     (lambda (str)
       (concat "  " str))
     cands
     "\n")
  )
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  ([remap find-file] . counsel-find-file)
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
  ;; NOTE: this variable do not work if defined in :custom
  (setq ivy-format-function 'ivy-format-function-pretty)
  (setq counsel-yank-pop-separator
      (propertize "\n────────────────────────────────────────────────────────\n"
             'face `(:foreground "#6272a4")))

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
     counsel-describe-function-function #'helpful-callable
     counsel-describe-variable-function #'helpful-variable
     ;; Add smart-casing (-S) to default command arguments:
     counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
     counsel-ag-base-command "ag -S --nocolor --nogroup %s"
     counsel-pt-base-command "pt -S --nocolor --nogroup -e %s"
    counsel-find-file-at-point t)

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
)

(use-package flx
  :ensure t
)

(use-package amx
:disabled
  :ensure t
)

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode 1)
)

(use-package ivy-posframe
  :ensure t
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe ((t (:background "#333244"))))
  (ivy-posframe-border ((t (:background "#abff00"))))
  (ivy-posframe-cursor ((t (:background "#00ff00"))))
  :hook
  (ivy-mode . ivy-posframe-mode)
  :config
  ;; custom define height of post frame per function
  (setq ivy-posframe-height-alist '((swiper . 20)
                                    (t      . 25)))

  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          ;;(counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (counsel-M-x     . ivy-posframe-display-at-frame-center)
          (t               . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1)
)

(use-package ivy-rich
:ensure t
:config
(ivy-rich-mode 1)
(setq ivy-format-function #'ivy-format-function-line)

;; use all-the-icons for `ivy-switch-buffer'
(defun ivy-rich-switch-buffer-icon (candidate)
   (with-current-buffer
    (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (if (symbolp icon)
      (all-the-icons-icon-for-mode 'fundamental-mode)
        icon))))
;; add the above function to `ivy-rich--display-transformers-list'
(setq ivy-rich--display-transformers-list
    '(ivy-switch-buffer
      (:columns
       ((ivy-rich-switch-buffer-icon :width 2)
        (ivy-rich-candidate (:width 30))
        (ivy-rich-switch-buffer-size (:width 7))
        (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
        (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
        (ivy-rich-switch-buffer-project (:width 15 :face success))
        (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
       :predicate
       (lambda (cand) (get-buffer cand)))))
)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind
  ;; ("M-x" . helm-M-x)
  ("C-c h" . helm-command-prefix)
  ("C-x b" . helm-buffers-list)
  ("C-x C-b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  ("C-x r b" . helm-bookmarks)
  ("M-y" . helm-show-kill-ring)
  ("M-:" . helm-eval-expression-with-eldoc)
  (:map helm-map
  ("C-z" . helm-select-action)
  ("C-h a" . helm-apropos)
  ("C-c h" . helm-execute-persistent-action)
  ("<tab>" . helm-execute-persistent-action)
  )
  :init
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-bookmark-show-location t)
  (setq helm-buffer-max-length 40)
  (setq helm-split-window-inside-p t)

  ;; turn on helm fuzzy matching
  (setq helm-M-x-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)

  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-follow-mode-persistent t)
  ;; take between 10-30% of screen space
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 30)
  :config
  (require 'helm-config)
  (helm-mode 1)
  ;; Make helm replace the default Find-File and M-x
  ;;(global-set-key [remap execute-extended-command] #'helm-M-x)
  ;; (global-set-key [remap find-file] #'helm-find-files)
  ;; helm bindings
  (global-unset-key (kbd "C-x c"))
)

(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init
  (setq helm-ag-insert-at-point 'symbol)
  (setq  helm-ag-command-option "--path-to-ignore ~/.agignore")
)

(use-package helm-rg
  :ensure t
  :defer t
)

(use-package helm-fuzzier
  :disabled nil
  :ensure t
  :after helm
  :config
  (helm-fuzzier-mode 1)
)

(use-package abbrev
  :ensure nil
  :config
  (define-abbrev-table 'global-abbrev-table '(
      ("alpha" "α")
      ("infinity" "∞")
      ("arrow" "→")
      ))
)

(use-package hydra
  :ensure t
)

(use-package major-mode-hydra
  :ensure t)

(use-package hydra-posframe
  :load-path "packages"
  :custom
  (hydra-posframe-parameters
    '((left-fringe . 5)
      (right-fringe . 5)))
  :custom-face
  (hydra-posframe-border-face ((t (:background "#6272a4"))))
  :hook (after-init . hydra-posframe-mode) ;; changed from `hydra-postframe-enable' cause emacs itself suggested
)

(use-package occur
  :ensure nil
  :config
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)
)

(use-package ag
  :ensure-system-package
  (ag . the_silver_searcher)
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag)
)

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
)

(use-package rg
  :ensure t
  :defer t
  :ensure-system-package
  (rg . ripgrep)
  :config
  ;; choose between default keybindings or magit like menu interface.
  ;; both options are mutually exclusive
  (rg-enable-default-bindings)
  ;;(rg-enable-menu)

)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer t
  :ensure-system-package
  ((tslint . "npm i -g tslint")
   (typescript . "npm i -g typescript"))
  :hook
  ;;(prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-display-errors-delay 1)
  :config
  ;;(global-flycheck-mode)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)

  ;; force flycheck to use its own xml parser instead of libxml32 (was giving me errors)
  (setq flycheck-xml-parser 'flycheck-parse-xml-region)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

)

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :custom-face
  (flycheck-posframe-face ((nil (:background "#20fabe" :foreground "#FFCC0E"))))
  (flycheck-posframe-info-face ((nil (:inherit 'info))))
  (flycheck-posframe-warning-face ((nil (:inherit 'warning))))
  (flycheck-posframe-error-face ((nil (:inherit 'error))))
  (flycheck-posframe-background-face ((nil (:background "#fcfa23" :foreground "#ff0000"))))
  (flycheck-posframe-border-face ((nil (:background "#af3ec8"))))
  :config
  (setq flycheck-posframe-position 'point-bottom-left-corner)
  (setq flycheck-posframe-prefix "\u27a4 ") ;; default: ➤
  (setq flycheck-posframe-warning-prefix "\u26a0 ")
  (setq flycheck-posframe-info-prefix "\uf6c8 ")
  (setq  flycheck-posframe-error-prefix "\u274c ")
  (setq flycheck-posframe-border-width 2)
  ;; Calling (flycheck-posframe-configure-pretty-defaults) will configure flycheck-posframe to show warnings and errors with nicer faces (inheriting from warning and error respectively), and set the prefix for each to nicer unicode characters.
  ;;(flycheck-posframe-configure-pretty-defaults)
)

(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode t)
  ;;:hook
  ;; this line was giving the `Wrong number of arguments error'
  ;; (after-save . (lambda () (magit-after-save-refresh-status t)))
  :bind
  ("<tab>" . magit-section-toggle)
  ("M-g s" . magit-status)
  ("C-x g" . magit-status)
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

)

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

(use-package diffview
  :ensure t
  :defer t
)

(use-package magit-todos
  :ensure t
  :after magit hl-todo
  :bind
  ("M-g t" . magit-todos-list)
  :config
  (magit-todos-mode)
)

(use-package git-messenger
  :ensure t
  :bind
  ("C-c g p" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t)
  :config
  (progn
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close))
)

;; Mode for .gitignore files.
(use-package gitignore-mode :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

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

(use-package ob-restclient
  :ensure t
  :mode "\\.rest$"
  :config
  ;; add restclient to org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)))
)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
;;  (undo-tree-mode)
)

(use-package corral
  :ensure t
  :bind
  ("M-9" . corral-parentheses-backward)
  :config
  (setq corral-preserve-point t)
  ;;(global-set-key (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key (kbd "M-[") 'corral-brackets-backward)
  (global-set-key (kbd "M-]") 'corral-brackets-forward)
  (global-set-key (kbd "M-{") 'corral-braces-backward)
  (global-set-key (kbd "M-}") 'corral-braces-forward)
  (global-set-key (kbd "M-\"") 'corral-double-quotes-backward)
)

(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

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

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
)



(use-package parinfer
  :ensure t
  :bind
  ("C-," . parinfer-toggle-mode)
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            ;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode))
    :config
    ;; auto switch to Indent Mode whenever parens are balance in Paren Mode
    (setq parinfer-auto-switch-indent-mode nil)  ;; default is nil
    (setq parinfer-lighters '(" Parinfer:Indent" . "Parinfer:Paren"))

)

(use-package elisp-format
  :ensure t
)

(use-package company
  :ensure t
  :diminish company-mode
  :defer t
  :init
  (global-company-mode)
  :bind
  (:map evil-insert-state-map
  ;; ("<tab>" . company-indent-or-complete-common)
  ("C-SPC" . company-indent-or-complete-common))
  (:map company-active-map
  ("M-n" . nil)
  ("M-p" . nil)
  ;; ("C-n" . company-select-next)
  ;; ("C-p" . company-select-previous)
  ("C-n" . company-select-next-or-abort)
  ("C-p" . company-select-previous-or-abort)
  ("<tab>" . company-complete-common-or-cycle)
  ("S-<backtab>" . company-select-previous)
  ("<backtab>" . company-select-previous)
  ("C-d" . company-show-doc-buffer))
  (:map company-search-map
  ;; ("C-p" . company-select-previous)
  ;; ("C-n" . company-select-next)
  ("C-p" . company-select-previous-or-abort)
  ("C-n" . company-select-next-or-abort))
  :config
  ;; define company appearance
  (custom-set-faces
  '(company-preview-common ((t (:foreground unspecified :background "#111111"))))
  '(company-scrollbar-bg ((t (:background "#111111"))))
  '(company-scrollbar-fg ((t (:background "#555555"))))
  '(company-tooltip ((t (:inherit default :background "#202029"))))
  '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
  '(company-tooltip-selection ((t (:inherit company-tooltip-common :background "#2a2a2a" )))))

  ;; Use Company for completion
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map))
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-minimum-prefix-length 1)               ; start completing after 1st char typed
  (setq company-idle-delay 0)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  ;; company-dabbrev
  (setq company-dabbrev-downcase nil)                  ;; Do not downcase completions by default.
  (setq company-dabbrev-ignore-case t)  ;; Even if I write something with the ‘wrong’ case, provide the ‘correct’ casing.
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-other-buffers t)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-selection-wrap-around t)               ; continue from top when reaching bottom
  (setq company-auto-complete 'company-explicit-action-p)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)
  (setq company-complete-number t)                     ;; Allow (lengthy) numbers to be eligible for completion.
  (setq company-show-numbers t)  ;; M-⟪num⟫ to select an option according to its number.
  (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
  ;; (setq company-tooltip-flip-when-above t)
  ;; DELETE THIS PART IF USE PACKAGE :MAP WORKS
  ;; (define-key company-active-map (kbd "M-n") nil)
  ;; (define-key company-active-map (kbd "M-p") nil)
  ;; (define-key company-active-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
)

(use-package company-emoji
  :disabled
  :ensure t
  :config
  (add-to-list 'company-backends 'company-emoji)
  )

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
   (setq company-quickhelp-delay 0.7)
   (company-quickhelp-mode)
)

(use-package company-posframe
  :ensure t
  :diminish company-posframe-mode
  :after company
  :config
  (company-posframe-mode 1)
  ;;:hook
  ;;(company-mode . company-posframe-mode)
  ;;(global-company-mode . company-posframe-mode)
)

;; (use-package company-box
  ;;   :ensure t
  ;;   :hook
  ;;   (company-mode . company-box-mode)
  ;;   (global-company-mode . company-box-mode)
  ;; )
(use-package company-box
  :ensure t
  :diminish company-box-mode
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
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

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go))
)

;; (use-package company-tabnine
;;   :demand
;;   :custom
;;   (company-tabnine-max-num-results 9)
;;   :bind
;;   (("M-q" . company-other-backend)
;;    ("C-z t" . company-tabnine))
;;   :config
;;   ;; Enable TabNine on default
;;   (add-to-list 'company-backends #'company-tabnine)

;;   ;; Integrate company-tabnine with lsp-mode
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-lsp
;;             candidates-tabnine)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-tabnine))
;;             (push candidate candidates-lsp)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-lsp (nreverse candidates-lsp))
;;         (setq candidates-tabnine (nreverse candidates-tabnine))
;;         (nconc (seq-take candidates-tabnine 3)
;;                (seq-take candidates-lsp 6)))))
;;   (add-hook 'lsp-after-open-hook
;;             (lambda ()
;;               (setq company-tabnine-max-num-results 3)
;;               (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;               (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-inhibit-message nil) ;; was `t`, changed to nil to see what it does
  (setq lsp-eldoc-render-all t)  ;; was `nil`, changed to nil to see what it does
  (setq lsp-highlight-symbol-at-point t)  ;; was `nil`, changed to nil to see what it does
  (setq lsp-print-io nil)
  (setq lsp-trace nil)
  (setq lsp-print-performance nil)
  (setq lsp-prefer-flymake t) ;; t(flymake), nil(lsp-ui), or :none
)

(use-package company-lsp
  :ensure t
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 10)
  (lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
  :config
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-async t)
  ;; When `company-lsp-cache-candidates' is setting is auto, company-lsp will filter the candidates client side without retrieving them from the server when you type. This means that the candidates might not be the same(e. g. might be sorted in a different order) but from a functional standpoint of view you should not notice any difference.
  ;;(setq company-lsp-cache-candidates t)
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-enable-recompletion t)
)

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  :preface
  (defun tau/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
      (progn
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc--hide-frame)
      )
    (lsp-ui-doc-mode 1)
    )
  )
  :config
  ;; lsp-ui appearance
  (set-face-attribute 'lsp-ui-doc-background  nil :background "#f9f2d9")
  (add-hook 'lsp-ui-doc-frame-hook
    (lambda (frame _w)
      (set-face-attribute 'default frame :font "Overpass Mono 11")))

  (set-face-attribute 'lsp-ui-sideline-global nil
                      :inherit 'shadow
                      :background "#f9f2d9")
  (setq ;; lsp-ui-doc
        lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'at-point ;; top, bottom, or at-point
        lsp-ui-doc-max-width 100
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
        lsp-ui-peek-list-width 40
        lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  :bind
    (:map lsp-mode-map
      ("C-c u f r" . lsp-ui-peek-find-references)
      ("C-c u f d" . lsp-ui-peek-find-definitions)
      ("C-c u f i" . lsp-ui-peek-find-implementation)
      ("C-c g d" . lsp-goto-type-definition)
      ("C-c f d" . lsp-find-definition)
      ("C-c g i" . lsp-goto-implementation)
      ("C-c f i" . lsp-find-implementation)
      ("C-c m"   . lsp-ui-imenu)
      ("C-c s"   . lsp-ui-sideline-mode)
      ("C-c d"   . tau/toggle-lsp-ui-doc)
    )
    ;; remap native find-definitions and references to use lsp-ui
    (:map lsp-ui-mode-map
      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
      ([remap xref-find-references] . lsp-ui-peek-find-references)
      ("C-c u" . lsp-ui-imenu))
)

;; (define-key ac-completing-map [return] nil)
;; (define-key ac-completing-map "\r" nil)

;(require 'auto-complete)
;(global-auto-complete-mode t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  ;;:hook
  ;;(prog-mode . yas-minor-mode)
  ;;(text-mode . yas-minor-mode)
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
  (yas-reload-all)
  ;; disabled global mode in favor or hooks in prog and text modes only
  (yas-global-mode 1)
)

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t
)

(use-package ivy-explorer
  :ensure t
  :after ivy
  :config
  ;; use ivy explorer for all file dialogs
  (ivy-explorer-mode 1)
)

(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-dwin-target t)
  (add-hook 'dired-initial-position-hook #'dired-k)
)

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode)
)

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
   :init
   ;; general variables
   (setq treemacs-no-png-images nil)
   (setq treemacs-deferred-git-apply-delay 0.5)
   (setq treemacs-display-in-side-window t)
   (setq treemacs-eldoc-display t)
   (setq treemacs-file-event-delay 5000)
   (setq treemacs-file-follow-delay 0.2)
   (setq treemacs-follow-after-init t)
   (setq treemacs-git-command-pipe "")
   (setq treemacs-goto-tag-strategy 'refetch-index)
   (setq treemacs-indentation 2)
   (setq treemacs-indentation-string " ")
   (setq treemacs-is-never-other-window nil)
   (setq treemacs-max-git-entries 5000)
   (setq treemacs-missing-project-action 'ask)
   (setq treemacs-no-delete-other-windows t)
   (setq treemacs-project-follow-cleanup nil)
   (setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
   (setq treemacs-position 'left)
   (setq treemacs-recenter-distance 0.1)
   (setq treemacs-recenter-after-file-follow nil)
   (setq treemacs-recenter-after-tag-follow nil)
   (setq treemacs-recenter-after-project-jump 'always)
   (setq treemacs-recenter-after-project-expand 'on-distance)
   (setq treemacs-show-cursor nil)
   (setq treemacs-show-hidden-files t)
   (setq treemacs-silent-filewatch nil)
   (setq treemacs-silent-refresh nil)
   (setq treemacs-sorting 'alphabetic-desc)
   (setq treemacs-space-between-root-nodes t)
   (setq treemacs-tag-follow-cleanup t)
   (setq treemacs-tag-follow-delay 1.5)
   (setq treemacs-width 35)
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

(use-package treemacs-evil
   :after treemacs evil
   :ensure t
)

(use-package treemacs-projectile
   :after treemacs projectile
   :ensure t
)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode)
)

(use-package treemacs-magit
   :after treemacs magit
   :ensure t
)

(use-package neotree
  :bind
  ("<f7>" . neotree-toggle)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-window-fixed-size nil)
    (evil-leader/set-key
      "tt" 'neotree-toggle
      "tp" 'neotree-projectile-action))

  ;; neotree 'icons' theme, which supports filetype icons
  (setq neo-theme (if (display-graphic-p) 'icons))
  (setq neo-theme 'icons)
  (setq neo-window-width 32)

  ;; Neotree bindings
  (add-hook 'neotree-mode-hook
            (lambda ()
              ; default Neotree bindings
              (define-key evil-normal-state-local-map (kbd "<tab>") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "|") 'neotree-enter-vertical-split)
              (define-key evil-normal-state-local-map (kbd "-") 'neotree-enter-horizontal-split)
              ; simulating NERDTree bindings in Neotree
              (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "u") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
              (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node))))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind
  ("<f6>" . dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
         (lambda ()
           (unless (file-remote-p default-directory)
             (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
)

(use-package ranger
  :ensure t
  :bind
  ("C-x C-j" . ranger)
  :config
  (setq ranger-show-hidden t) ;; show hidden files
)

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

(use-package rotate
  :ensure t
  :bind
  ("C-c r w" . rotate-window)
  ("C-c r l" . rotate-layout)
)

(setq echo-keystrokes 0.02)

;; Hilight trailing whitespace
;; like this -->
;;
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "orange1")

(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

(setq display-time-24hr-format t)
(display-time-mode +1)

;; appearantly the `inhibit-splash-screen' was deprecaded. uses `inhibit-startup-screen' now
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)
;; Makes *scratch* empty.
(setq initial-scratch-message nil)
;; Don't show *Buffer list* when opening multiple files at the same time.
;;(setq initial-major-mode 'org-mode)  ;;start in org-mode
(setq inhibit-startup-buffer-menu t)
;; Make the buffer that opens on startup your init file ("~/.emacs" or
;; "~/.emacs.d/init.el").
;;(setq initial-buffer-choice user-init-file)

(blink-cursor-mode t)
(setq blink-cursor-blinks 0) ;; blink forever
(setq-default indicate-empty-lines t)

(setq frame-title-format '("Emacs"))

(scroll-bar-mode -1)

(tool-bar-mode -1)
(menu-bar-mode -1)

(set-background-color "#111111")
(set-foreground-color "#dddddd")

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

(defun load-my-theme (frame)
  "Function to load the theme in current FRAME.
  sed in conjunction
  with bellow snippet to load theme after the frame is loaded
  to avoid terminal breaking theme."
  (select-frame frame)
  (load-theme my-theme t))

; make emacs load the theme after loading the frame
; resolves issue with the theme not loading properly in terminal mode on emacsclient
;; NOTE this if was breaking my emacs!!!!!
;; (add-hook 'after-make-frame-functions #'load-my-theme)

(use-package doom-themes
  :ensure t
  :disabled
  :init (load-theme 'doom-tomorrow-night t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (doom-themes-treemacs-config)
)

(use-package uniquify
  :defer 1
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t)
)

(use-package all-the-icons
  :ensure t
)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file)
)

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

(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode)
)

(use-package dimmer
  :disabled
  :ensure t
  :config (dimmer-mode)
)

(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode)
)

(dolist (frame (frame-list))
  (set-frame-parameter frame 'bottom-divider-width 1)
  (set-frame-parameter frame 'right-divider-width 1))


           (push (cons 'bottom-divider-width 1) default-frame-alist)
           (push (cons 'right-divider-width 1) default-frame-alist)

(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

(defconst tau-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p tau-savefile-dir)
  (make-directory tau-savefile-dir))

(use-package desktop
:ensure nil
:bind
("C-c s" . desktop-save-in-desktop-dir)
:init
;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(setq desktop-restore-eager 5) ;; restore 5 buffers immediately. the others restore lazily
(setq desktop-load-locked-desktop t)
(setq desktop-files-not-to-save "^$")
(setq desktop-save t)
(setq desktop-buffers-not-to-save
     (concat "\\("
             "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
             "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
       "\\)$"))
:config
(desktop-save-mode t)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'completion-list-mode)

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
    '(lambda ()
       ;; desktop-remove clears desktop-dirname
       (setq desktop-dirname-tmp desktop-dirname)
       (desktop-remove)
       (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
    (desktop-save-in-desktop-dir)
  (message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
    '(lambda ()
       (if (saved-session)
     (if (y-or-n-p "Restore desktop? ")
         (session-restore)))))
)

(use-package auto-save
  :ensure nil
  :config
  (setq auto-save-default nil)  ;; dont auto save files
)

(use-package saveplace
  :ensure nil
  :config
  (defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

  ;; create the savefile dir if it doesn't exist
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir))

  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t)
  (save-place-mode t)
)

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

(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1)
)

(use-package auto-save-visited
  :ensure nil
  :config
  (auto-save-visited-mode)
)

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

(use-package avy
  :ensure t
  :bind
  ("M-g j" . avy-goto-char-2)
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ;; replace native M-g g `goto-line' with `avy-goto-line'
  ("M-g f" .  avy-goto-line)
  ("M-g g" .  avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  (:map isearch-mode-map
  ("C-'" . avy-search))
  (:map evil-normal-state-map
  ("SPC" . avy-goto-char))
  (:map evil-visual-state-map
  ("SPC" . avy-goto-char))
  :config
  (setq avy-background t) ;; default nil ;; gray background will be added during the selection.
  (setq avy-highlight-first t) ;; When non-nil highlight the first decision char with avy-lead-face-0. Do this even when the char is terminating.

  ;; nil: use only the selected window
  ;; t: use all windows on the selected frame
  ;; all-frames: use all windows on all frames
  (setq avy-all-windows nil) ;;
)

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

(use-package rotate
  :ensure t
  :bind
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

(setq mode-line-format
      (list
       ;; value of `mode-name'
       "%m: "
       ;; value of current buffer name
       "buffer %b, "
       ;; value of current line number
       "line %l "
       "-- user: "
       ;; value of user
       (getenv "USER"))
)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(use-package show-point-mode
  :ensure nil
  :load-path "packages/show-point-mode"
  :config
  (show-point-mode t)
)

(set-face-attribute 'mode-line nil :height tau/font-size-mode-line)
(set-face-attribute 'mode-line nil
                    :background "#007ad3"
                    :foreground "#ffffff"
                    :box '(:line-width 4 :color "#007ad3") ;; modeline border
                    :overline nil
                    :underline nil)

;; for now the inactive modeline looks the same as the active one
(set-face-attribute 'mode-line-inactive nil
                    :background "#007ad3"
                    :foreground "#ffffff"
                    :box '(:line-width 4 :color "#007ad3") ;; modeline border
                    :overline nil
                    :underline nil)

(use-package anzu
  :ensure t
  :bind
  (:map isearch-mode-map
  ([remap isearch-query-replace] . anzu-isearch-query-replace)
  ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  (global-anzu-mode +1)

  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 50)
  (setq anzu-replace-to-string-separator " => ")

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)

;;  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
;;  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
)

(use-package hide-mode-line
  :ensure t
  :hook
  (completion-list-mode . hide-mode-line-mode)
  (neotree-mode . hide-mode-line-mode)
  (treemacs-mode . hide-mode-line-mode)
)

(use-package saveplace
  :ensure t
  :hook
  (after-init . save-place-mode)
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
)

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

(use-package feebleline
  :ensure t
  :disabled
  :config
  (setq feebleline-msg-functions
        '((feebleline-line-number         :post "" :fmt "%5s")
          (feebleline-column-number       :pre ":" :fmt "%-2s")
          (feebleline-file-directory      :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (feebleline-file-modified-star  :face font-lock-warning-face :post "")
          (feebleline-git-branch          :face feebleline-git-face :pre " : ")
          (feebleline-project-name        :align right)))
  (feebleline-mode 1)
)

(use-package lunar-mode-line
  :ensure nil
  :load-path "packages/lunar-mode-line"
)

(use-package celestial-mode-line
  :ensure t
  :disabled
  :config
  (setq calendar-longitude 25.5)
  (setq calendar-latitude 17.5)
  (setq calendar-location-name "Some place")
  ;; Icons customization
  (defvar celestial-mode-line-phase-representation-alist '((0 . "○") (1 . "☽") (2 . "●") (3 . "☾")))
  (defvar celestial-mode-line-sunrise-sunset-alist '((sunrise . "☀↑") (sunset . "☀↓")))
  ;; add to end of global-mode-string
  (if (null global-mode-string)
      (setq global-mode-string '("" celestial-mode-line-string))
  (add-to-list 'global-mode-string 'celestial-mode-line-string t))
  ;; Start the timer, to update every few minutes:
  (celestial-mode-line-start-timer)
)

(use-package common-header-mode-line
  :load-path "packages/common-header-mode-line"
  :disabled
  :ensure nil
  ;;:hook
  ;; (after-init . (lambda () (common-header-line-mode 1)))
  ;; (after-init . (lambda () (common-mode-line-mode 1)))
  :config
  ;;(with-eval-after-load "common-header-mode-line-autoloads"
  ;;  (common-mode-line-mode 1)
  ;;  (common-header-line-mode 1))
)

(use-package minions
  :ensure t
  :config
  (minions-mode 1)
)

(use-package doom-modeline
  :ensure t
  :init
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 20)                  ;; modeline height. only respected in GUI
  (setq doom-modeline-bar-width 3)                ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-icon t)                     ;; display icons in the modeline
  (setq doom-modeline-major-mode-icon t)          ;; display the icon for the major mode. it respects `doom-modeline-icon'
  (setq doom-modeline-major-mode-color-icon t)    ;; display color icons for `major-mode'. It respects `doom-modeline-icon' and `all-the-icons-color-icons'.
  (setq doom-modeline-buffer-state-icon t)        ;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
  (setq doom-modeline-buffer-modification-icon t) ;; Whether display buffer modification icon. It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-minor-modes nil)            ;; Whether display minor modes in mode-line or not.
  (setq doom-modeline-enable-word-count nil)      ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-buffer-encoding t)          ;; Whether display buffer encoding.
  (setq doom-modeline-indent-info nil)            ;; Whether display indentation information.
  (setq doom-modeline-checker-simple-format t)    ;; If non-nil, only display one number for checker information if applicable.
  (setq doom-modeline-vcs-max-length 12)          ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-persp-name t)               ;; Whether display perspective name or not. Non-nil to display in mode-line.
  (setq doom-modeline-persp-name-icon nil)        ;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
  (setq doom-modeline-lsp t)                      ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-github nil)                 ;; Whether display github notifications or not. Requires `ghub` package.
  (setq doom-modeline-github-interval (* 30 60))  ;; The interval of checking github.
  (setq doom-modeline-mu4e t)                     ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-irc t)                      ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc-stylize 'identity)      ;; Function to stylize the irc buffer names.

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
)

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

(use-package nyan-mode
   :if window-system
   :hook
   (after-init . nyan-mode)
   :config
   (setq nyan-cat-face-number 4)
   (setq nyan-animate-nyancat t)
   (setq nyan-wavy-trail t)
   (nyan-start-animation))

(require 'discover)
(when (featurep 'discover)
  (discover-add-context-menu
    :context-menu '(isearch
              (description "Isearch, occur and highlighting")
              (lisp-switches
               ("-cf" "Case should fold search" case-fold-search t nil))
              (lisp-arguments
               ("=l" "context lines to show (occur)"
                "list-matching-lines-default-context-lines"
                (lambda (dummy) (interactive) (read-number "Number of context lines to show: "))))
              (actions
               ("Isearch"
                ("_" "isearch forward symbol" isearch-forward-symbol)
                ("w" "isearch forward word" isearch-forward-word))
               ("Occur"
                ("o" "occur" occur))
               ("More"
                ("h" "highlighters ..." makey-key-mode-popup-isearch-highlight))))
    :bind "M-s"
  )

  (discover-add-context-menu
    :context-menu '(dired)
    :bind "?"
    :mode 'dired-mode
    :mode-hook 'dired-mode-hook
  )
)

(global-set-key (kbd "C-M-+") 'balance-windows-area)

(use-package zoom
  :ensure t
  :bind
  ("C-M-z" . zoom)
  :init
  (setq zoom-size '(0.618 . 0.618))
  (setq zoom-ignored-major-modes '(treemacs dired-mode neotree dired-sidebar))
  (setq zoom-ignored-buffer-names '("zoom.el" "init.el"))
  (setq zoom-ignored-buffer-name-regexps '("^*calc"))
  :config
  (zoom-mode t)
)

(use-package sublimity
  :ensure t
  :disabled
  :config
  (sublimity-mode 1)
)

(use-package sublimity-map
  :disabled
  :ensure nil
  :config
  (setq sublimity-map-size 14)  ;; minimap width
  (setq sublimity-map-fraction 0.3)
  (setq sublimity-map-text-scale -5)
  (sublimity-map-set-delay nil) ;; minimap is displayed after 5 seconds of idle time

  ;; document this snippet better, not sure what it does, but it defines the font-family
;;  (add-hook 'sublimity-map-setup-hook
;;          (lambda ()
;;            (setq buffer-face-mode-face '(:family "Monospace"))
;;            (buffer-face-mode)))

)

(use-package sublimity-attractive
  :disabled
  :ensure nil
  :config
  (setq sublimity-attractive-centering-width 110)

  ;; these are functions (NOT variables) to configure some UI parts
  ;; (sublimity-attractive-hide-bars)
  ;; (sublimity-attractive-hide-vertical-border)
  ;; (sublimity-attractive-hide-fringes)
  ;; (sublimity-attractive-hide-modelines)
)

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
)

(use-package dashboard
  :ensure t
  :bind ("C-S-D" . open-dashboard)
  :hook
  (after-init . dashboard-setup-startup-hook)
  :preface
  (defun tau/dashboard-banner ()
    "Sets a dashboard banner including information on package initialization
     time and garbage collections."
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time
                   (time-subtract after-init-time before-init-time)) gcs-done)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :init

  ;; set widgets to show
  (setq dashboard-items '((recents  . 5)
                         (bookmarks . 5)
                         (projects . 5)
                         (agenda . 5)
                         (registers . 5))
  )


  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; sets dashboard as emacs initial buffer on startup

  ;; Set the title
  (setq dashboard-banner-logo-title "Hi 😊 ")
  (setq dashboard-banner-logo-title
          (message " ★ Emacs initialized in %.2fs ★ "
                   (float-time (time-subtract (current-time) my-init-el-start-time))))


  (setq dashboard-startup-banner 'logo) ;; Set the banner ;; values: ('oficial, 'logo, 1, 2, 3, or "path/to/image.png")
  (setq dashboard-center-content t) ;; Content is not centered by default. To center, set
  (setq dashboard-show-shortcuts t) ;; To disable shortcut "jump" indicators for each section, set

  ;; To add icons to the widget headings and their items:
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (setq dashboard-set-navigator t) ;; To show navigator below the banner
  (setq dashboard-set-init-info t) ;;To show info about the packages loaded and the init time:

  ;; A randomly selected footnote will be displayed. To disable it:
  ;;(setq dashboard-set-footer nil)

  :config
  ;; Org mode’s agenda
  ;; To display today’s agenda items on the dashboard, add agenda to dashboard-items:
  (add-to-list 'dashboard-items '(agenda) t)
  ;; To show agenda for the upcoming seven days set the variable show-week-agenda-p to t.
  (setq show-week-agenda-p t)
  ;; Note that setting list-size for the agenda list is intentionally ignored; all agenda items for the current day will be displayed.
  ;; To customize which categories from the agenda items should be visible in the dashboard set the dashboard-org-agenda-categories to the list of categories you need.
  (setq dashboard-org-agenda-categories '("Tasks" "Appointments"))

  ;; adds fireplace as a widget
;;  (defun dashboard-insert-custom (list-size)
;;    (fireplace))
;;  (add-to-list 'dashboard-item-generators  '(fireplace . dashboard-insert-custom))
;;  (add-to-list 'dashboard-items '(fireplace) t)


)



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

(use-package minimap
  :ensure t
  :disabled t
  :commands
  (minimap-bufname minimap-create minimap-kill)
  :custom
  (minimap-major-modes '(prog-mode))
  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 20)
  :bind
  ("M-<f12> m" . tau/toggle-minimap)
  :preface
  (defun tau/toggle-minimap ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill)))
  :config
  (custom-set-faces
   '(minimap-active-region-background
    ((((background dark)) (:background "#555555555555"))
      (t (:background "#C847D8FEFFFF"))) :group 'minimap))
)

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

(global-prettify-symbols-mode 1)
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
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

(use-package centaur-tabs
  :ensure t
  :hook
  (after-init . centaur-tabs-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :custom
  ;; appearantly these dont work if put in :init
  (centaur-tabs-style "bar")           ; types available: (alternative, bar, box, chamfer, rounded, slang, wave, zigzag)
  (centaur-tabs-height 28)
  (centaur-tabs-set-icons t)           ;; display themed icons from all the icons
  (centaur-tabs-set-modified-marker t) ;; display a marker indicating that a buffer has been modified (atom-style)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-set-close-button t)
  (centaur-tabs-close-button "X")
  (centaur-tabs-set-bar 'over)         ;; in previous config value was 'over
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-adjust-buffer-order t)
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  :custom-face
  (centaur-tabs-active-bar-face ((t (:background "cyan"))))
  (centaur-tabs-default ((t (:background "black" :foreground "black"))))
  (centaur-tabs-unselected ((t (:background "#292929" :foreground "grey50"))))
  (centaur-tabs-selected ((t (:background "#181818" :foreground "white"))))
  (centaur-tabs-unselected-modified ((t (:background "#3D3C3D" :foreground "grey50"))))
  (centaur-tabs-selected-modified ((t (:background "#181818" :foreground "white"))))
  (centaur-tabs-close-unselected ((t (:inherit centaur-tabs-unselected))))
  (centaur-tabs-close-selected ((t (:inherit centaur-tabs-selected))))
  (centaur-tabs-close-mouse-face ((t (:inherit underline))))
  (centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected))))
  (centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected))))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
  ("g t" . centaur-tabs-forward)
  ("g T" . centaur-tabs-backward))
;;  :init
  :config
  ;; functions
  ;; (centaur-tabs-change-fonts "arial" 160)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-mode t)
)

(use-package hideshowvis
  :ensure nil
  :load-path "packages"
  :hook
  (display-line-numbers-mode . hideshowvis-enable)
  :config
  (hideshowvis-symbols) ; displaying a + symbol in the fringe for folded regions
)

(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :config
  (set-face-background 'show-paren-mismatch "red")
  (set-face-background 'show-paren-match "#4445e0")
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (show-paren-mode +1)
)

(use-package color-identifiers-mode
  :ensure t
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

(use-package highlight-numbers
  :ensure t
  :hook
  (prog-mode . highlight-numbers-mode)
)

(use-package highlight-operators
  :ensure t
  :hook
  (prog-mode . highlight-operators-mode)
)

(use-package highlight-escape-sequences
  :ensure t
  :hook
  (prog-mode . hes-mode)
)

(use-package highlight-parentheses
  :ensure t
  :hook
  (prog-mode . highlight-parentheses-mode)
)

(use-package diff-hl
  :ensure t
    :custom-face (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-mode)
  (magit-post-refresh . diff-hl-mode)
  :init
  ;; (add-hook 'prog-mode-hook #'diff-hl-mode)
  ;; (add-hook 'org-mode-hook #'diff-hl-mode)
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; Better looking colours for diff indicators
  (custom-set-faces
    '(diff-hl-change ((t (:background "#3a81c3"))))
    '(diff-hl-insert ((t (:background "#7ccd7c"))))
    '(diff-hl-delete ((t (:background "#ee6363"))))
  )

  :config

  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (setq diff-hl-side 'left)
  (setq diff-hl-margin-side 'left)
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

  (global-diff-hl-mode 1) ;; Enable diff-hl globally
)

;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)
  :init
  ;; (add-hook 'text-mode-hook (lambda () (hl-todo-mode t)))
  :config
  ;; Adding a new keyword: TEST.
  (add-to-list 'hl-todo-keyword-faces '("TODO" . "#ff3300"))
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "#dc8cc3"))
  (add-to-list 'hl-todo-keyword-faces '("NOTE" . "#ffff00"))
  (add-to-list 'hl-todo-keyword-faces '("DONE" . "#00ff00"))
)

(use-package hi-lock
  :init
  (global-hi-lock-mode 1)
  :config
  (add-hook 'hi-lock-mode-hook
          (lambda nil
            (highlight-regexp "FIXME" 'hi-red-b)
            (highlight-regexp "NOTE" 'hi-red-b)
            (highlight-regexp "TODO" 'hi-red-b))
  )
  ;; always highlight patterns found in files without confirmation
  (setq hi-lock-file-patterns-policy #'(lambda (dummy) t))
)

(use-package hl-anything
  :ensure t
  :after evil
;;  :hook
;;  (kill-emacs . hl-save-highlights)
  :bind
  ("C-<f8> h" . hl-highlight-thingatpt-local)
  ("C-<f8> S-h" . hl-highlight-thingatpt-global)
  ("C-<f8> u l" . hl-unhighlight-all-local)
  ("C-<f8> u g" . hl-unhighlight-all-global)
  ("C-<f8> n" . hl-find-next-thing)
  ("C-<f8> p" . hl-find-prev-thing)
  ("C-<f8> s" . hl-save-highlights)
  ("C-<f8> r" . hl-restore-highlights)
  :config
  (hl-highlight-mode 1)

  ;; evil leader key bindings for hl-anything
  (evil-leader/set-key
    "hul"  'hl-unhighlight-all-local
    "hug" 'hl-unhighlight-all-global
    "htg" 'hl-highlight-thingatpt-global
    "htl"  'hl-highlight-thingatpt-local
    "hn"  'hl-find-next-thing
    "hp"  'hl-find-prev-thing
    "hr"  'hl-restore-highlights
    "hs"  'hl-save-highlights)
)

(use-package highlight-tail
  :load-path "packages/highlight-tail"
  :ensure nil
  :config
  (setq highlight-tail-colors '(("#9310FF" . 0) ;; closest to cursor
                                 ("#9370DB" . 35)  ;; midle of tail
                                 ("#DDA0DD" . 76)))  ;; end of the tail
  (setq highlight-tail-steps 17)
  (setq highlight-tail-timer 0.05)
  (setq highlight-tail-posterior-type 'const)
  (highlight-tail-mode)
)

(use-package beacon
  :ensure t
  :hook
  (post-self-insert . beacon-blink)
  (blink-cursor-mode. beacon-blink)
  (after-change-functions . beacon-blink)
  (delete-selection-mode . beacon-blink)
  (normal-erase-is-backspace . beacon-blink)
  :init
  (setq inhibit-modification-hooks nil)
  (setq beacon-blink-when-point-moves-vertically 1) ; default nil
  (setq beacon-blink-when-point-moves-horizontally 1) ; default nil
  (setq beacon-blink-when-buffer-changes t) ; default t
  (setq beacon-blink-when-window-scrolls t) ; default t
  (setq beacon-blink-when-window-changes t) ; default t
  (setq beacon-blink-when-focused nil) ; default nil
  (setq beacon-blink-duration 0.2) ; default 0.3
  (setq beacon-blink-delay 0.3) ; default 0.3
  (setq beacon-size 23) ; default 40
  (setq beacon-color "#9310FF") ; default 0.5
  :config
  (setq inhibit-modification-hooks nil)
  (add-hook 'after-change-functions #'beacon-blink)
  (beacon-mode 1)
)

(use-package rainbow-delimiters
  :load-path "packages/highlight-tail"
  :ensure nil
  ;;:hook
  ;;(emacs-lisp-mode . rainbow-delimiters-mode)
  ;;(prog-mode . rainbow-delimiters-mode)
)

(use-package rainbow-mode
  :ensure t
)

(use-package hl-line
  :ensure nil
  :defer nil
  :config
  (global-hl-line-mode)
)

(use-package col-highlight
  :disabled
  :defer nil
  :config
  (col-highlight-toggle-when-idle)
  (col-highlight-set-interval 2)
)

(use-package crosshairs
  :disabled
  :defer nil
  :config
  (crosshairs-mode)
)

(use-package volatile-highlights
  :ensure t
  :disabled
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  :config
  ;;-----------------------------------------------------------------------------
  ;; Supporting evil-mode.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                        'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  ;;-----------------------------------------------------------------------------
  ;; Supporting undo-tree.
  ;;-----------------------------------------------------------------------------
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
)

(use-package highlight-indent-guides
  :ensure t
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character) ; column
)

(use-package highlight-context-line
  :ensure t
  :disabled
  :config
  (highlight-context-line-mode 1)
)

;;(setq redisplay-dont-pause t)
(setq scroll-preserve-screen-position 1)  ;; centered screen scrolling
  ;; (setq scroll-margin 10
  ;; (setq maximum-scroll-margin 0.5)
  ;; (setq scroll-step 1)
  ;; (setq scroll-conservatively 10000) ;; scroll one line at a time when you move the cursor past the top or bottom of the window
  ;; (setq scroll-step 1) ;; keyboard scroll one line at a time

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-scroll-amount '(1 ((control) . 5)))
  (setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
)

(global-set-key (kbd "<S-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<S-mouse-5>") 'scroll-up-line)

(use-package sublimity-scroll
  :ensure nil
  :config
  (setq sublimity-scroll-weight 10)  ;; default 10
  (setq sublimity-scroll-drift-length 5)  ;; default 5
  (setq sublimity-scroll-hide-cursor t) ;; default t
)

(use-package scrollkeeper
:disabled
:ensure t
:bind
([remap scroll-up-command] . scrollkeeper-contents-up)
([remap scroll-down-command] . scrollkeeper-contents-down)
)

(use-package fast-scroll
:disabled
:ensure t
;; If you would like to turn on/off other modes, like flycheck, add
;; your own hooks.
:init
(setq fast-scroll-throttle 0.5)
:config
(add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
(add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
(fast-scroll-config)
(fast-scroll-mode 1)
)

(use-package centered-cursor-mode
  :ensure t
  :disabled
  :config
  (global-centered-cursor-mode)
)

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-min-display-lines 3)
  (setq which-key-max-description-length 20)
  (setq which-key-max-display-columns 6)
)

(use-package keyfreq
  :ensure t
  :hook (after-init . keyfreq-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

(use-package smartparens
   :ensure t
   ;;:hook
   ;;(after-init . smartparens-global-mode)
   :config
   (require 'smartparens-config)
   (sp-pair "=" "=" :actions '(wrap))
   (sp-pair "+" "+" :actions '(wrap))
   (sp-pair "<" ">" :actions '(wrap))
   (sp-pair "$" "$" :actions '(wrap))
)

(use-package evil-smartparens
  :ensure t
  :hook
  (smartparens-enabled . evil-smartparens-mode)
)

(use-package smartscan
  :ensure t
  :config
  (smartscan-mode 1)
)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1)
)

(use-package multiple-cursors
  :after evil
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

(use-package quickrun
  :bind
  ("C-<f5>" . quickrun)
  ("M-<f5>" . quickrun-shell)
)

(use-package tex
  :ensure auctex
  :mode ("\\.tex" . latex-mode)
  :defer t
  :hook
  (LaTeX-mode . turn-on-visual-line-mode)
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

(add-to-list 'org-latex-classes
	      '("beamer"
	        "\\documentclass\[presentation\]\{beamer\}"
	        ("\\section\{%s\}" . "\\section*\{%s\}")
	        ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
	        ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))
)

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

(add-to-list 'org-latex-classes
             '("abntex2"
               "\\documentclass{abntex2}"
               ;; ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}"))
)

(use-package sgml-mode
  :ensure nil
  :hook
  ;;(html-mode . (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))
  (sgml-mode . aggressive-indent-mode)
  (sgml-mode . rainbow-mode)
  (sgml-mode . emmet-mode)
  :init
  (setq sgml-basic-offset 4)
  :config
  (add-hook 'html-mode-hook
    (lambda ()
      (set (make-local-variable 'sgml-basic-offset) 4)))
)

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :hook
  (css-mode . aggressive-indent-mode)
  (css-mode . prettier-js-mode)
  (css-mode . emmet-mode)
  :init
  (setq css-indent-offset 2)
)

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
   (add-to-list 'auto-mode-alist '("\\.component.scss$\\'" . scss-mode))
)

(use-package helm-css-scss
  :ensure t
  :after helm
  :bind
  (:map isearch-mode-map
  ("s-i" . helm-css-scss-from-isearch))
  (:map helm-css-scss-map
  ("s-i" . helm-css-scss-multi-from-helm-css-scss))
  (:map css-mode-map
  ("s-i" . helm-css-scss)
  ("s-S-I" . helm-css-scss-back-to-last-point))
  (:map scss-mode-map
  ("s-i" . helm-css-scss)
  ("s-S-I" . helm-css-scss-back-to-last-point))
  :config
  (setq helm-css-scss-insert-close-comment-depth 2
        helm-css-scss-split-with-multiple-windows t
        helm-css-scss-split-direction 'split-window-vertically)

  ;; Set local keybind map for css-mode / scss-mode / less-css-mode
  (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
    (add-hook
     $hook (lambda ()
             (local-set-key (kbd "s-i") 'helm-css-scss)
             (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))
)

(use-package yaml-mode
  :ensure t
)

(use-package toml-mode
  :ensure t
)

(use-package emmet-mode
  :ensure t
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

(use-package ruby-mode
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

;; Functions to help with refactoring
(use-package ruby-refactor
  :ensure t
  :defer t
  :hook
  (ruby-mode . ruby.refactor-mode-launch)
)

(use-package ruby-hash-syntax
  :ensure t
)

(use-package ruby-additional
  :ensure t
)

(use-package ruby-tools
  :ensure t
)

(use-package rspec-mode
  :ensure t
)

(use-package ruby-block
  :disabled
  :ensure t
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

(use-package ruby-extra-highlight
  :ensure t
  :hook
  (ruby-mode . ruby-extra-highlight-mode)
)

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
)

(use-package web-mode
  :after flycheck company
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
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
  :hook
  (web-mode . rainbow-mode)
  (web-mode . flycheck-mode)
  (web-mode . company-mode)
  (web-mode . emmet-mode)
  (web-mode . editorconfig-mode)
  (web-mode . color-identifiers-mode)
  (web-mode . aggressive-indent-mode)
  :config
  ;; Template
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))

  ;; web-mode indentation
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)

  ;;=======================================
  ;; Flycheck Setup for JavaScript
  ;;---------------------------------------
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  ;; enable eslint as default js flycheck linter
  (setq flycheck-checkers '(javascript-eslint))
  ;; use eslint_d insetad of eslint for faster linting
  (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; add flycheck linter for webmode
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Use tidy to check HTML buffers with web-mode.
  (flycheck-add-mode 'html-tidy 'web-mode)
  ;;(eval-after-load 'flycheck
  ;;   '(flycheck-add-mode 'html-tidy 'web-mode))

  ;;=======================================
  ;; Flycheck Setup for TSX
  ;;---------------------------------------
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

)

(use-package web-beautify
  :ensure t
  :ensure-system-package
  (js-beautify . "npm i -g js-beautify")
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer)
)

(use-package web-mode-edit-element
  :ensure t
)

(use-package js2-mode
  :after flycheck company
  :mode
  ("\\.js$" . js2-mode)
  :hook
  (js2-mode . flycheck-mode)
  (js2-mode . company-mode)
  ;;(js2-mode . tide-mode)
  (js2-mode . add-node-modules-path)
  (js2-mode . lsp)
  (js2-jsx-mode . lsp)
  (js2-mode . rainbow-mode)
  (js2-mode . color-identifiers-mode)
  (js2-mode . prettier-js-mode)
  (js2-mode . aggressive-indent-mode)
  (js2-mode . indium-interaction-mode)
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
  ;; use eslint_d insetad of eslint for faster linting
  (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; set modes that will use ESLint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)

  ;; Workaround for eslint loading slow
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  ;;=======================================

  )

(use-package js2-refactor
  :ensure t
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

(use-package xref-js2
  :ensure t
  :config
  ;;(setq xref-js2-search-program 'rg)

  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook (lambda ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
)

(use-package indium
  :ensure t
  :ensure-system-package
  (indium . "npm i -g indium")
  :after js2-mode typescript-mode
  :bind
  (:map js2-mode-map
  ("C-c C-l" . indium-eval-buffer))
  (:map typescript-mode-map
  ("C-c C-l" . indium-eval-buffer))
  :config
  (setq indium-update-script-on-save t)
)

(use-package add-node-modules-path
  :ensure t
)

(use-package json-snatcher
  :hook
  (json-mode . js-mode-bindings)
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
)

(use-package js-import
  :ensure t
)

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :ensure-system-package
  (prettier . "npm install -g prettier")
  :init
  (setq prettier-js-show-errors 'buffer) ;; options: 'buffer, 'echo or nil
  :config
  (setq prettier-js-args '("--bracket-spacing" "false"
                           "--print-width" "80"
                           "--tab-width" "2"
                           "--use-tabs" "false"
                           "--no-semi" "true"
                           "--single-quote" "true"
                           "--trailing-comma" "none"
                           "--no-bracket-spacing" "true"
                           "--jsx-bracket-same-line" "false"
                           "--arrow-parens" "avoid"))

  ;; use prettier from local `node_modules' folder if available
  (defun tau/use-prettier-if-in-node-modules ()
    "Enable prettier-js-mode iff prettier was found installed locally in project"
    (interactive)
    (let* ((file-name (or (buffer-file-name) default-directory))
           (root (locate-dominating-file file-name "node_modules"))
           (prettier (and root
                          (expand-file-name "node_modules/prettier/bin-prettier.js" root))))
      (if (and prettier (file-executable-p prettier))
          (progn
            (message "Found local prettier executable at %s. Enabling prettier-js-mode" prettier)
            (setq prettier-js-command prettier)
            (make-variable-buffer-local 'prettier-js-command)
            (prettier-js-mode)
            (message "Disabling aggressive-indent-mode in favour of prettier")
            (aggressive-indent-mode -1))
        (progn
          (message "Prettier not found in %s. Not enabling prettier-js-mode" root)
          (message "Falling back to aggressive-indent-mode")
          (aggressive-indent-mode 1)))))
  ;; disabled cause my current project doenst have prettier local
  ;; (add-hook 'prettier-js-mode-hook #'tau/use-prettier-if-in-node-modules)

)

(use-package format-all
:ensure t
:bind ("C-c C-f" . format-all-buffer)
)

(use-package json-mode
  :ensure t
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2))

(use-package eslintd-fix
  :ensure t
  :ensure-system-package
  (eslint . "npm i -g eslint")
  :config
  ;; Grab eslint executable from node_modules instead of global
  ;; Taken from https://github.com/flycheck/flycheck/issues/1087#issuecomment-246514860
  ;; Gist: https://github.com/lunaryorn/.emacs.d/blob/master/lisp/lunaryorn-flycheck.el#L62
  (defun lunaryorn-use-js-executables-from-node-modules ()
    "Set executables of JS and TS checkers from local node modules."
    (-when-let* ((file-name (buffer-file-name))
                 (root (locate-dominating-file file-name "node_modules"))
                 (module-directory (expand-file-name "node_modules" root)))
      (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                             (javascript-eslint . "eslint")
                                             (typescript-tslint . "tslint")
                                             (javascript-jscs   . "jscs")))
        (let ((package-directory (expand-file-name module module-directory))
              (executable-var (flycheck-checker-executable-variable checker)))
          (when (file-directory-p package-directory)
            (set (make-local-variable executable-var)
                 (expand-file-name (if (string= module "tslint")
                                       (concat "bin/" module)
                                     (concat "bin/" module ".js"))
                                   package-directory)))))))
)

(use-package rjsx-mode
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

(use-package typescript-mode
  :ensure t
  :after indium
  :mode
  (("\\.ts\\'" . typescript-mode)
   ("\\.tsx\\'" . typescript-mode))
  :hook (
  (typescript-mode . tide-mode)
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (typescript-mode . eldoc-mode)
  (typescript-mode . aggressive-indent-mode)
  (typescript-mode . prettier-js-mode)
  (typescript-mode . turn-on-visual-line-mode)
  (typescript-mode . editorconfig-mode)
  (typescript-mode . indium-interaction-mode)
  (typescript-mode . smartparens-mode)
  )
)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :preface
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
  )
  :hook
  (tide-mode . setup-tide-mode)
  (before-save . tide-format-before-save)
  :init
  (setq tide-always-show-documentation t)
  :config
  ;;(add-hook 'before-save-hook 'tide-format-before-save)
  ;;(add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;;(add-hook 'js2-mode-hook #'setup-tide-mode)
)

(use-package ng2-mode
  :defer
  :disabled
  :mode
  ("\\.component.ts$\\'" "\\.component.html$\\'")
  :hook
  (ng2-mode . rainbow-mode)
  (ng2-mode . flycheck-mode)
  (ng2-mode . company-mode)
  (ng2-mode . smartparens-mode)
  (ng2-mode . editorconfig-mode)
  (ng2-mode . color-identifiers-mode)
  (ng2-mode . lsp-mode)
  (ng2-mode . prettier-js-mode)
  (ng2-mode . eldoc-mode)
)

(use-package format-all
:ensure t
:bind ("C-c C-f" . format-all-buffer)
)

(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2)

  ;;Flycheck setup for JSON Mode
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
)

(use-package rjsx-mode
    :after js2-mode
    :mode
    ("\\.jsx$" . rjsx-mode)
    ("components/.+\\.js$" . rjsx-mode)

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

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
)

(use-package cheat-sh
  :ensure t
)

(defun animated-self-insert ()
  (let* ((undo-entry (car buffer-undo-list))
         (beginning (and (consp undo-entry) (car undo-entry)))
         (end (and (consp undo-entry) (cdr undo-entry)))
         (str (when (and (numberp beginning)
                       (numberp end))
                (buffer-substring-no-properties beginning end)))
         (animate-n-steps 3))
    (when str
      (delete-region beginning end)
      (animate-string str (1- (line-number-at-pos)) (current-column)))))

;; to disable simply comment this hook
;;; (add-hook 'post-self-insert-hook 'animated-self-insert)

(use-package c-c-combo
  :ensure t
)

(use-package xkcd
:ensure t
)

(use-package fireplace
  :ensure t
  :init (defvar fireplace-mode-map)
  :bind (:map fireplace-mode-map
              ("d" . fireplace-down)
              ("s" . fireplace-toggle-smoke)
              ("u" . fireplace-up))
  :config
  (setq fireplace-toggle-smoke t)
  ;; (fireplace)
)

(use-package selectric-mode
  :ensure t
)

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))
(define-key tetris-mode-map (kbd "C-p") 'tetris-rotate-prev)
(define-key tetris-mode-map (kbd "C-n") 'tetris-move-down)
(define-key tetris-mode-map (kbd "C-b") 'tetris-move-left)
(define-key tetris-mode-map (kbd "C-f") 'tetris-move-right)
(define-key tetris-mode-map (kbd "C-SPC") 'tetris-move-bottom)
(defadvice tetris-end-game (around zap-scores activate)
  (save-window-excursion ad-do-it))

(use-package pacmacs
  :ensure t
)

(use-package epaint
  :if window-system
  :commands (epaint)
  :init
  (with-eval-after-load (quote epaint-context)
    (unless (boundp (quote cl-struct-epaint-drawable))
      (defvar cl-struct-epaint-drawable (quote epaint-drawable)))
    (unless (boundp (quote cl-struct-epaint-gc))
      (defvar cl-struct-epaint-gc (quote epaint-gc))))
)

(use-package speed-type
  :defer t)

(use-package 2048-game
:defer t)

(use-package zone
 :ensure nil
 :defer 5
 :config
 ;; (zone-when-idle 600) ; in seconds
 (defun zone-choose (pgm)
   "Choose a PGM to run for `zone'."
   (interactive
    (list
     (completing-read
      "Program: "
      (mapcar 'symbol-name zone-programs))))
   (let ((zone-programs (list (intern pgm))))
     (zone))))

(use-package meme
  :ensure nil
  :commands (meme meme-file)
)

(use-package zone-nyan
  :ensure t
)

(use-package zone-nyan
  :ensure t
)

(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;;(global-set-key (kbd "M-S-D") 'duplicate-line)
(global-set-key [(meta shift d)] 'duplicate-line)

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
