(setq user-full-name "Gustavo P Borges")
(setq user-mail-address "gugutz@gmail.com")
(setq work-mail-address "gugutz@stairs.studio")
(setq gmail-address "gugutz@gmail.com")
(setq nickname "gugutz")

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

;; Faster startup
(setq gc-cons-threshold 100000000)

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
(set-face-attribute 'default nil :font (font-candidate '"Hack-10:weight=normal"
                                                        "Consolas-10:weight=normal"
                                                        "Droid Sans Mono-10:weight=normal"
                                                        "DejaVu Sans Mono-10:weight=normal"
                                                        "Ubuntu Mono-12:weight=normal"))

(use-package visual-line-mode
  :ensure nil
  :hook
  (prog-mode . visual-line-mode)
  (text-mode . visual-line-mode)
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
  :hook
  ;; (prog-mode. display-line-numbers-mode)
  ;; (text-mode. display-line-numbers-mode)
  (LaTeX-mode. display-line-numbers-mode)
  (typescript-mode. display-line-numbers-mode)
  (web-mode. display-line-numbers-mode)
  (js2-mode. display-line-numbers-mode)
  (css-mode. display-line-numbers-mode)
  (scss-mode. display-line-numbers-mode)
  (elixir-mode. display-line-numbers-mode)

  :config
  ;; (global-display-line-numbers-mode)
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)

  ;; old linum-mode variables, check if they work with new display-line-numbers-mode
  ;; (setq linum-format 'dynamic)
  ;; (setq linum-format " %d ") ;; one space separation between the linenumber display and the buffer contents:
  ;; (setq linum-format "%4d “) ;; 4 character and a space for line numbers
  (setq linum-format "%4d \u2502 ") ; 4 chars and a space with solid line separator



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

;; (require 'expand-region)
(global-set-key (kbd "C-S-<tab>") 'er/expand-region)

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; Kill current buffer; prompt only if
;; there are unsaved changes.
(global-set-key (kbd "C-x k")
  '(lambda () (interactive) (kill-buffer (current-buffer)))
)

(use-package smooth-scrolling
    :ensure t
    :config
    (smooth-scrolling-mode 1)
)

;; Vertical Scroll
;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101)
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; (; Horizontal Scroll
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)

(setq require-final-newline t)

(setq-default fill-column 80)

(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding

(setq window-combination-resize t)

;; (setq-default line-spacing 1) ;; A nice line height
(setq-default line-spacing 3)

(setq system-uses-terminfo nil) ;; Fix weird color escape sequences

;; (setq confirm-kill-emacs 'yes-or-no-p) ;; Ask for confirmation before closing emacs

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

(use-package aggressive-indent
  :ensure t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (css-mode . aggressive-indent-mode)
  :config
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
(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

(use-package move-text
  :ensure t
  :bind
  ("M-S-J" . move-text-up)
  ("M-S-K" . move-text-down)
  (:map evil-normal-state-map
  ("S-J" . move-text-up)
  ("S-K" . move-text-down)
  )
  (:map evil-visual-state-map
  ("M-S-J" . move-text-region-up)
  ("M-S-K" . move-text-region-down)
  )
  :config
  (move-text-default-bindings)
)

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
  :defer t
  :hook
  (text-mode . guess-language-mode)
  ;; :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_US" "English"))
                                   (br . ("pt_BR" "Portuguese Brazilian"))
                                  )
  guess-language-languages '(en br)
  guess-language-min-paragraph-length 45)
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
(require 'epa-file)
(epa-file-enable)

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
  (right-click-context-menu))))

(use-package hippie-exp
  ;;:ensure nil
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
          )
  )
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
  ("C-r" . undo-tree-redo)
  )
  (:map evil-insert-state-map
  ;; this is also defined globally above in the config
  ("C-S-<tab>" . er/expand-region)
  )
  (:map evil-visual-state-map
  ;; this is also defined globally above in the config
  ("<tab>" . indent-region)
  ("C-/" . comment-line)
  ("C-S-/" . comment-region)
  ("C-S-M-/" . comment-box)
  ("M-=" . #'align-values)
  )

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
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; recover native emacs commands that are overriden by evil
  ;; this gives priority to native emacs behaviour rathen than Vim's
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
  (define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
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

(use-package evil-numbers
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
  ("C-c +" . evil-numbers/inc-at-pt)
  ("C-c -" . evil-numbers/dec-at-pt)
  ("<kp-add>" . evil-numbers/inc-at-pt)
  ("<kp-subtract>" . evil-numbers/dec-at-pt))
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
)

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" 'find-file
    "q" 'evil-quit
    "w" 'save-buffer
    "d" 'delete-frame
    "k" 'kill-buffer
    "b" 'switch-to-buffer
    "-" 'split-window-bellow
    "|" 'split-window-right)
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
  (setq evil-goggles-duration 0.100) ;; default is 0.200

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
  :defer t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switch)
  ;; this map is to delete de bellow commented lambda that does the same thing
  ;; Resolve issue with Tab not working with ORG only in Normal VI Mode in terminal
  ;; (something with TAB on terminals being related to C-i...)
  (:map evil-normal-state-map
  ("<tab>" . org-cycle)
  )
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
          org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

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

(use-package evil-org
  :after org
  :hook
  (org-mode . evil-org-mode)
  :config
  (lambda ()
    (evil-org-set-key-theme))
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
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)

(use-package org-jira
  :ensure t
  :defer 3
  :after org
  :custom
  (jiralib-url "https://stairscreativestudio.atlassian.net")
)

(use-package ox-jira
  :defer 3
  :after org
)

(use-package ox-reveal
  :after ox
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
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

(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("M-x" . helm-M-x)
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
  (global-set-key [remap execute-extended-command] #'helm-M-x)
  (global-set-key [remap find-file] #'helm-find-files)
  ;; helm bindings
  (global-unset-key (kbd "C-x c"))
)

(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-rg
  :ensure t
  :defer t
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

(use-package helm-fuzzier
  :disabled nil
  :ensure t
  :after helm
  :config
  (helm-fuzzier-mode 1)
)

(use-package flycheck
    :ensure t
    :defer t
    :hook
    (prog-mode . flycheck-mode)
    :custom
    (flycheck-display-errors-delay 1)
    :config
    (global-flycheck-mode)

    ;; add eslint to list of flycheck checkers
    (setq flycheck-checkers '(javascript-eslint))
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
    ;; force flycheck to use its own xml parser instead of libxml32 (was giving me errors)
    (setq flycheck-xml-parser 'flycheck-parse-xml-region)
    ;; set modes that will use ESLint
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'js-mode)

    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))

    ;; Workaround for eslint loading slow
    ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
)

(use-package quick-peek
  :ensure t
)

(use-package flycheck-inline
  :ensure t
  :hook
  (flycheck-mode . flycheck-inline-mode)
  :config
  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)

  (setq flycheck-mode-line-prefix "Syntax")

  ;; (global-flycheck-inline-mode)
  (setq flycheck-inline-display-function
        (lambda (msg pos)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide))

(use-package flycheck-pos-tip
  :ensure t
  :disabled
  :defines flycheck-pos-tip-timeout
  :after flycheck
  :hook
  (global-flycheck-mode . flycheck-pos-tip-mode)
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode)
)

;;; Show Flycheck errors in tooltip
(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-popup-tip-error-prefix "\u27a4") ;;  display arrow like this: `➤'
  ;; (setq flycheck-popup-tip-error-prefix "* ")
)

(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status)
  ("C-x g" . magit-status)
)

(use-package magit-todos
  :ensure t
  :after magit
  :after hl-todo
  :bind
  ("M-g t" . magit-todos-list)
  :config
  (magit-todos-mode)
)

(use-package evil-magit
  :ensure t
  :init
;;  (evil-magit-init)
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  :config
  (evil-define-key evil-magit-state magit-mode-map "j" 'magit-log-popup)
  (evil-define-key evil-magit-state magit-mode-map "k" 'evil-next-visual-line)
  (evil-define-key evil-magit-state magit-mode-map "l" 'evil-previous-visual-line)
)

(use-package diff-hl
  :ensure t
    :custom-face (diff-hl-change ((t (:foreground ,(face-background 'highlight)))))
  :hook
  (prog-mode . diff-hl-mode)
  (org-mode . diff-hl-mode)
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

(use-package projectile
  :ensure t
  :bind
  (:map projectile-mode-map
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  )
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

(use-package restclient
  :ensure t
  :mode
  ("\\.rest$\\'" "\\.http$\\'")
  :config
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
  :init
  (global-undo-tree-mode)
;;  (undo-tree-mode)
)

(use-package corral
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
  :defer t
  :init
  (global-company-mode)
  :bind
  (:map evil-insert-state-map
  ;; ("<tab>" . company-indent-or-complete-common)
  ("C-SPC" . company-indent-or-complete-common)
  )
  (:map company-active-map
  ("M-n" . nil)
  ("M-p" . nil)
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)
  ("<tab>" . company-complete-common-or-cycle)
  ("S-<tab>" . company-select-previous)
  ("<backtab>" . company-select-previous)
  ("C-d" . company-show-doc-buffer)
  )
  (:map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
  )
  :config
  ;; define company appearance
  (custom-set-faces
  '(company-preview-common ((t (:foreground unspecified :background "#111111"))))
  '(company-scrollbar-bg ((t (:background "#111111"))))
  '(company-scrollbar-fg ((t (:background "#555555"))))
  '(company-tooltip ((t (:inherit default :background "#222222"))))
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
  :hook
  (company-mode . company-posframe-mode)
  (global-company-mode . company-posframe-mode)
)

;; (use-package company-box
  ;;   :ensure t
  ;;   :hook
  ;;   (company-mode . company-box-mode)
  ;;   (global-company-mode . company-box-mode)
  ;; )
(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face)))))
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

  :hook
  ;; disabled lsp for javascript and typescript to use Tide-mode only
  ;; disabled lsp for typescript to use Tide-mode only
  ;;(typescript-mode . lsp)
  ;;(js2-mode . lsp)
  (js2-jsx-mode . lsp)
  (enh-ruby-mode . lsp)
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
  (lsp-prefer-flymake t) ;; t(flymake), nil(lsp-ui), or :none
  :config
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-async t)
  (setq company-lsp-cache-candidates t)
  (setq company-lsp-enable-recompletion t)
)

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
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
      (set-face-attribute 'default frame :font "Overpass Mono 11")
    )
  )
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
      ("C-c C-r" . lsp-ui-peek-find-references)
      ("C-c C-j" . lsp-ui-peek-find-definitions)
      ("C-c g d" . lsp-goto-type-definition)
      ("C-c f d" . lsp-find-definition)
      ("C-c g i" . lsp-goto-implementation)
      ("C-c f i" . lsp-find-implementation)
      ("C-c i"   . lsp-ui-peek-find-implementation)
      ("C-c m"   . lsp-ui-imenu)
      ("C-c s"   . lsp-ui-sideline-mode)
      ("C-c d"   . ladicle/toggle-lsp-ui-doc)
    )
    ;; remap native find-definitions and references to use lsp-ui
    (:map lsp-ui-mode-map
      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
      ([remap xref-find-references] . lsp-ui-peek-find-references)
      ("C-c u" . lsp-ui-imenu)
    )
)

;; (define-key ac-completing-map [return] nil)
;; (define-key ac-completing-map "\r" nil)

(require 'auto-complete)
(global-auto-complete-mode t)

(use-package yasnippet
  :ensure t
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
  ;; (with-eval-after-load 'yasnippet
  ;;  (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (yas-reload-all)
  ;; disabled global mode in favor or hooks in prog and text modes only
  ;; (yas-global-mode 1)
)

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t
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
   (setq treemacs-no-png-images nil)
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
                                   (setq buffer-face-mode-face `(:background "#211C1C"))
                                   (buffer-face-mode 1)))

   ;;(treemacs-resize-icons 44) ;; usefull on high dpi monitors.  default icon size is 22

   (treemacs-follow-mode t)
   (treemacs-filewatch-mode t)
   (treemacs-fringe-indicator-mode t)
   (pcase (cons (not (null (executable-find "git")))
                (not (null treemacs-python-executable)))
     (`(t . t)
      (treemacs-git-mode 'deferred))
     (`(t . _)
      (treemacs-git-mode 'simple)))

)

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

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

(use-package rotate
  :ensure t
  :bind
  ("C-c C-r w" . rotate-window)
  ("C-c C-r l" . rotate-layout)
)

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-." . ace-jump-mode)
)

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

(use-package edit-server
  :if (and window-system
           (not alternate-emacs))
  ;; :if window-system
  :ensure t
  :defer 5
  :disabled
  :config
  (edit-server-start)
)

(use-package atomic-chrome
  :ensure t
  :disabled
  :config
  (atomic-chrome-start-server)
)

;; appearantly the `inhibit-splash-screen' was deprecaded. uses `inhibit-startup-screen' now
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)
;; Makes *scratch* empty.
(setq initial-scratch-message nil)
;; Don't show *Buffer list* when opening multiple files at the same time.
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

(use-package uniquify
  :defer 1
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t)
)

(use-package all-the-icons
  :defer 3
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

(use-package sublimity
  :ensure t
  :config
  (setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)
  ;; this is the only part of the config where i use `use-package' inside another package config.
  ;; the oficial docs appears to suggest this way
  (sublimity-mode 1)
)

(use-package sublimity-scroll
  :ensure nil
  :disabled
  :config
  (setq sublimity-scroll-weight 10)
  (setq sublimity-scroll-drift-length 5)
)

(use-package sublimity-map
  :disabled t
  :ensure nil
  :config
  (setq sublimity-map-size 18)  ;; minimap width
  (setq sublimity-map-fraction 0.3)
  (setq sublimity-map-text-scale -7)
  (sublimity-map-set-delay 4) ;; minimap is displayed after 5 seconds of idle time

  ;; document this snippet better, not sure what it does, but it defines the font-family
  (add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))

)

(use-package sublimity-attractive
  :disabled nil
  :ensure nil
  :config
  (setq sublimity-attractive-centering-width 110)

  ;; these are functions (NOT variables) to configure some UI parts
  ;; (sublimity-attractive-hide-bars)
  ;; (sublimity-attractive-hide-vertical-border)
  ;; (sublimity-attractive-hide-fringes)
  ;; (sublimity-attractive-hide-modelines)
)

(use-package fancy-battery
  :ensure t
  :config
  (add-hook 'after-init-hook #'fancy-battery-mode)
)

(use-package dimmer
  :ensure t
  :config (dimmer-mode)
)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

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

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
)

(use-package dashboard
  :ensure t
  :after fireplace ;; testing to add fireplace to dashboard, need to be loaded first
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
                         (fireplace . 1)
                         (registers . 5))
  )

  ;; sets dashboard as emacs initial buffer on startup
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  ;; Set the title
  (setq dashboard-banner-logo-title "Hi 😊 ")
  (setq dashboard-banner-logo-title
          (message " ★ Emacs initialized in %.2fs ★ "
                   (float-time (time-subtract (current-time) my-init-el-start-time))))

  ;; Set the banner
  (setq dashboard-startup-banner 'logo) ;; values: ('oficial, 'logo, 1, 2, 3, or "path/to/image.png")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)

  ;; To add icons to the widget headings and their items:
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; To show navigator below the banner:
  (setq dashboard-set-navigator t)

  ;;To show info about the packages loaded and the init time:
  (setq dashboard-set-init-info t)

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
  (defun dashboard-insert-custom (list-size)
    (fireplace))
  (add-to-list 'dashboard-item-generators  '(fireplace . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(fireplace) t)

  (dashboard-setup-startup-hook)
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
  :commands
  (minimap-bufname minimap-create minimap-kill)
  :custom
  (minimap-major-modes '(prog-mode))
  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 20)
  :bind
  ("M-<f12> m" . ladicle/toggle-minimap)
  :preface
  (defun ladicle/toggle-minimap ()
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
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
  ("g t" . centaur-tabs-forward)
  ("g T" . centaur-tabs-backward))
  :init
  (setq centaur-tabs-style "rounded") ; types available: (alternative, bar, box, chamfer, rounded, slang, wave, zigzag)
  (setq centaur-tabs-height 25)
  (setq centaur-tabs-set-icons t) ;; display themed icons from all the icons
  (setq centaur-tabs-set-modified-marker t) ;; display a marker indicating that a buffer has been modified (atom-style)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-set-bar 'over) ;; in previous config value was 'over

  (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  :config
  ;; (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-change-fonts "arial" 160)
  (centaur-tabs-mode t)
)

(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :config
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (show-paren-mode +1)
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

(use-package beacon
  :ensure t
  :init
  (setq beacon-blink-when-point-moves-vertically nil) ; default nil
  (setq beacon-blink-when-point-moves-horizontally nil) ; default nil
  (setq beacon-blink-when-buffer-changes t) ; default t
  (setq beacon-blink-when-window-scrolls t) ; default t
  (setq beacon-blink-when-window-changes t) ; default t
  (setq beacon-blink-when-focused nil) ; default nil
  (setq beacon-blink-duration 0.3) ; default 0.3
  (setq beacon-blink-delay 0.3) ; default 0.3
  (setq beacon-size 20) ; default 40
  ;; (setq beacon-color "yellow") ; default 0.5
  (setq beacon-color 0.6) ; default 0.5
  :config
  (beacon-mode 1)
)

(use-package rainbow-delimiters
  :ensure t
  ;;:hook
  ;;(emacs-lisp-mode . rainbow-delimiters-mode)
  ;;(prog-mode . rainbow-delimiters-mode)
)

(use-package rainbow-mode
  :ensure t
  :hook
  (org-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode)
  (php-mode . rainbow-mode)
  (html-mode . rainbow-mode)
  (web-mode . rainbow-mode)
  (js2-mode . rainbow-mode))

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

(use-package which-key
  :hook (after-init . which-key-mode))
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-min-display-lines 3)
  (setq which-key-max-description-length 20)
  (setq which-key-max-display-columns 6)

(use-package keyfreq
  :disabled
  :ensure t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

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
  (smartparens-enabled . evil-smartparens-mode)
)

(smartscan-mode 1)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
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

(use-package auctex-latexmk
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup)
)

(use-package company-auctex
  :ensure t
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'company-auctex-init)
)

(use-package tex
  :ensure auctex
  :defer t
  :hook
  (LaTeX-mode . visual-line-mode)
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
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}"))
)

(add-hook 'html-mode-hook
  (lambda ()
    (set (make-local-variable 'sgml-basic-offset) 4)))

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :init
  (setq css-indent-offset 2)
)

(use-package scss-mode
  :ensure t
  ;; this mode doenst load using :mode from use-package, dunno why
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.component.scss\\'" . scss-mode))
  :init
  (setq scss-compile-at-save 'nil)
  :config
   (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
   (add-to-list 'auto-mode-alist '("\\.component.scss\\'" . scss-mode))
)

(use-package helm-css-scss
  :ensure t
  :bind (
  (:map isearch-mode-map
  ("s-i" . helm-css-scss-from-isearch)
  :map helm-css-scss-map
  ("s-i" . helm-css-scss-multi-from-helm-css-scss)
  :map css-mode-map
  ("s-i" . helm-css-scss)
  ("s-S-I" . helm-css-scss-back-to-last-point)
  :map scss-mode-map
  ("s-i" . helm-css-scss)
  ("s-S-I" . helm-css-scss-back-to-last-point))
  )
  :config
  (setq helm-css-scss-insert-close-comment-depth 2
        helm-css-scss-split-with-multiple-windows t
        helm-css-scss-split-direction 'split-window-vertically)
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
  :hook
    (sgml-mode . emmet-mode) ;; Auto-start on any markup modes
    (css-mode . emmet-mode) ;; enable Emmet's css abbreviation.
    (scss-mode . emmet-mode) ;; enable Emmet's css abbreviation.
    (html-mode . emmet-mode) ;; Auto-start on HTML files
    (web-mode . emmet-mode) ;; Auto-start on web-mode
  :config
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  (setq emmet-expand-jsx-className? t)) ;; use emmet with JSX markup

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
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
  "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'")
  :config
  ;; web-mode indentation
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Use tidy to check HTML buffers with web-mode.
  (eval-after-load 'flycheck
     '(flycheck-add-mode 'html-tidy 'web-mode))
)

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  ("\\.js$" . js2-mode)

  :hook
  (js2-mode . flycheck-mode)
  (js2-mode . company-mode)
  (js2-mode . tide-mode)
  (js2-mode . add-node-modules-path)
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js-chain-indent t)

  ;; use eslint_d insetad of eslint for faster linting
  ;; (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
)

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :mode
  ("\\.js$" . prettier-js-mode)
  ("\\.scss$" . prettier-js-mode)
  :ensure-system-package
  (prettier . "npm install -g prettier")
  :hook
  (js2-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (rjsx-mode . prettier-js-mode)
  (css-mode . prettier-js-mode)
  (scss-mode . prettier-js-mode)
  (json-mode . prettier-js-mode)
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
  (add-hook 'prettier-js-mode-hook #'tau/use-prettier-if-in-node-modules)

)

;; json-mode: Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode
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

    :config
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
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
)

(use-package tide
  :ensure t
  :config
  (progn
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
  )
)

(use-package ng2-mode
  :defer
  :hook
  (ng2-mode . prettier-js-mode)
)

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
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
