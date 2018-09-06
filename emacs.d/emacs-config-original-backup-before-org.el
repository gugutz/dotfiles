;;; package --- Summary
; -*- mode: elisp -*-

;########################################################
;;; Commentary:


;########################################################
;;; Code:

;;********************************************
;; adding package repositories
;; and initializing packages
;;********************************************

(require 'package)
; add melpa stable emacs package repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(package-initialize)

; moved this part to beggining of the file because if the
; "custom-safe-themes variable is not set before smart-mode-line (sml) activates
; emacs asks 2 annoying confirmations on every startup before actually starting

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


;;********************************************
; environment setup
;;********************************************

; allow access from emacsclient
(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

; prevent emacs to create lockfiles (.#file.file)
; this also stops preventing editing colisions, so watch out
(setq create-lockfiles nil)


; always follow symbolic links to edit the 'actual' file it points to
(setq vc-follow-symlinks t)

; enable mouse support in terminal mode
(when (eq window-system nil)
  (xterm-mouse-mode 1))

; Save all tempfiles in $TMPDIR/emacs$UID/
    (defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
    (setq backup-directory-alist
        `((".*" . ,emacs-tmp-dir)))
    (setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
        emacs-tmp-dir)
        

      
; disable the annoying Emacs bell ring (beep)
(setq ring-bell-function 'ignore)

;################################################
;;; PACKAGES
;################################################


;;********************************************
;; Requiring my packages
;;********************************************

(require 'web-mode)
(require 'auto-complete)

;;********************************************
;; Requiring my personal elisp configs
;;********************************************

; add the folder 'config' to emacs load-path
; so i can require stuff from there
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'eshell.tau)
(require 'evil.tau)
(require 'org.tau)
(require 'latex.tau)
;; (require 'ruby.tau)


;########################################################
; general text editing customizations
;########################################################

; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

; Line Number : Pretty format
(setq linum-format " %d ")

;; make emacs use clipboard
(setq x-select-enable-clipboard t)


;########################################################
; general code editing customizations
;########################################################

; parentheses
(show-paren-mode t)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)


;********************************************
; Enable rainbow-mode on relevant filetypes
; Colorize hex, rgb and named color codes
;********************************************
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)


;********************************************
; window navigation with vim-like bindings
;********************************************

(require 'evil-tmux-navigator)

;; for some readon the bellow lines should be the default native way for navigation on emacs
;; but they dont work
;; using the above package instead til i find a solution
;
;; (windmove-default-keybindings 'control)
;; (global-set-key (kbd "C-h") 'windmove-left)
;; (global-set-key (kbd "C-l") 'windmove-right)
;; (global-set-key (kbd "C-k") 'windmove-up)
;; (global-set-key (kbd "C-j") 'windmove-down)

;########################################################
; APPEARANCE
;########################################################


;********************************************
; Applying my theme
;********************************************


(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")
(require 'base16-theme)
; theme options:
; atom-one-dark (doenst work well with emacsclient, ugly blue bg)
; dracula
; darktooth
; gruvbox-dark-hard
; gruvbox-dark-light
; gruvbox-dark-medium
; base16-default-dark <-- this one is good

(setq my-theme 'dracula)

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

; this if was breaking my emacs!!!!!
; (if (or (daemonp) (server-running-p))
  (add-hook 'after-make-frame-functions #'load-my-theme)
  ;; (load-theme my-theme t)


;********************************************
; Customizing the mode line
;********************************************



(require 'smart-mode-line)
(if (require 'smart-mode-line nil 'noerror)
    (progn
      ;( sml/name-width 20)
      ;( sml/mode-width 'full)
      ;( sml/shorten-directory t)
      ;( sml/shorten-modes t)
      (require 'smart-mode-line-powerline-theme)
      ; this must be BEFORE (sml/setup)
      (sml/apply-theme 'powerline)
      ;; Alternatives:
      ;; (sml/apply-theme 'powerline)
      ;; (sml/apply-theme 'dark)
      ;; (sml/apply-theme 'light)
      ;; (sml/apply-theme 'respectful)
      ;; (sml/apply-theme 'automatic)


      (if after-init-time
          (sml/setup)
        (add-hook 'after-init-hook 'sml/setup))


      (display-time-mode 1)
      
      (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
      (add-to-list 'sml/replacer-regexp-list
                   '("^~/.*/lib/ruby/gems" ":GEMS" ))
      (add-to-list 'sml/replacer-regexp-list
                   '("^~/Projects/" ":CODE:"))))


;################################################
; General text editing settings
;################################################

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

; I recommend adding this to your .emacs, as it makes C-n insert newlines if the point is at the end of the buffer. Useful, as it means you won’t have to reach for the return key to add newlines!
(setq next-line-add-newlines t)

;; Smartscan mode
;; Usage:
;M-n and M-p move between symbols
;M-' to replace all symbols in the buffer matching the one under point
;C-u M-' to replace symbols in your current defun only (as used by narrow-to-defun.)
(smartscan-mode 1)

;; PDF Tools
(pdf-tools-install)

; Make buffer refresh every 1 second to PDF-tools updates the changed pdf
(setq auto-revert-interval 0.5)

; PDF tools evil keybindings
(evil-define-key 'normal pdf-view-mode-map
  "h" 'pdf-view-previous-page-command
  "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
  "l" 'pdf-view-next-page-command)

;################################################
;; MINOR MODES
;################################################

;; js2-refactor

(add-hook 'js2-mode-hook #'js2-refactor-mode)
; choose js2-refactor keybinding scheme (this can be changed easily)
(js2r-add-keybindings-with-prefix "C-c C-m")

; add prettier to js2, web and rjsx minor modes
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;#######################################
; FILE ASSOCIATIONS
;#######################################

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; template engines filetypes
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

; use rjsx mode for js files in React folder structure
; better support for JSX and React and GatsbyJs
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))

;********************************************
; magit
;********************************************

; define global keybing to magit-status
(global-set-key (kbd "C-x g") 'magit-status)

; FlyCheck linter
(add-hook 'after-init-hook #'global-flycheck-mode)

; enable autocompletion engine
(global-auto-complete-mode t)

;********************************************
; Languages specific settings
;********************************************

; Ruby mode
;(require 'ruby.tau)
;(add-to-list 'auto-mode-alist '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;PHP mode
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


; Go mode
; (autoload 'go-mode "go-mode" "Major mode for editing Go code." t)
; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

; ; markdown-mode
; (autoload 'mardown-mode "markdown-mode")
; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; ; haskell-mode
; ; (require 'haskell-interactive-mode)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (eval-after-load 'flycheck
;                  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
; (add-hook 'haskell-mode-hook (lambda ()
;                                (electric-indent-mode -1)))
; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
; (add-hook 'haskell-mode-hook (lambda () (global-set-key (kbd "<f5>") 'haskell-process-cabal-build)))



;########################################################
; HELPER FUNCTIONS
;########################################################

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
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(global-set-key [f9] 'copy-to-clipboard)
(global-set-key [f10] 'paste-from-clipboard)


(defun my-save ()
  "Save file when leaving insert mode in Evil."
  (if (buffer-file-name)
      (evil-save))
  )

(add-hook 'evil-insert-state-exit-hook 'my-save)



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:


(provide 'emacs)
;;; .emacs ends here