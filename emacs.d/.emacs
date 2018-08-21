; -*- mode: elisp -*-

;********************************************
; general emacs customizations
;********************************************

; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))


; add the folder 'config' to emacs load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

; always follow symbolic links to edit the 'actual' file it points to
(setq vc-follow-symlinks t)

; enable mouse support in terminal mode
(when (eq window-system nil)
      (xterm-mouse-mode t))

;********************************************
; Package repositories
;********************************************

(require 'package)

; add melpa stable emacs package repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


(package-initialize)


;********************************************
; Packages specific settings
;********************************************

;  To enable evil-commentary permanently, add

(evil-commentary-mode)
;;;; Neotree
;; use neotree (NERDTree for emacs) and toggle with F8
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
; neotree 'icons' theme, which supports filetype icons
(unless (display-graphic-p)
  (setq neo-theme 'icons))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

; make neotree window open and go the file currently opened
(setq neo-smart-open t)


; solve keybinding conflicts between neotree with evil mode
(add-hook 'neotree-mode-hook
          (lambda ()
            ; default Neotree bindings
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
            ; simulating NERDTree bindings in Neotree
            (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "u") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
            (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)))

; the following lines is how to do the same thing as above in recent versions of Evil Mode
    ; (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    ; (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    ; (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    ; (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    ; (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    ; (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    ; (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    ; (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    ; (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

    ; (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-change-root)
    ; (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-refresh)


;********************************************
; Emacs Appearance settings
;********************************************

; all the icons plugins
(require 'all-the-icons)

(add-to-list 'custom-theme-load-path "themes")
; make emacs load the theme after loading the frame
; to avoid not loading the theme in terminal mode on emacsclient
(setq my-theme 'dracula)
(defun load-my-theme (frame)
  (select-frame frame)
  (load-theme my-theme t))

; if daemon is running or running in server-mode, load theme after frame creation
; resolves issues with theme in emacsclient
(if (or (daemonp) (server-running-p))
  (add-hook 'after-make-frame-functions #'load-my-theme)
  (load-theme my-theme t))

;********************************************
; Customizing the mode line
;********************************************
;this must be BEFORE (sml/setup)
; (setq sml/theme 'dark)
;load smart-mode-line
; (sml/setup)

;********************************************
; General text editing settings
;********************************************

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer


;********************************************
; Require and use packages
;********************************************


;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


; enable Evil mode
(require 'evil)
(evil-mode 1)

; define global keybing to magit-status
(global-set-key (kbd "C-x g") 'magit-status)

; emacs multiple cursors keybindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;********************************************
; Vim-Like bindings
;********************************************

; imitate vim multiple selection behavior with multiple-cursors package
(define-key evil-normal-state-map (kbd "C-n") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "M-N") 'mc/mark-previous-like-this)

; evil-leader <leader> key package for Evil Mode
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "q" 'evil-quit
  "w" 'save-buffer
  "k" 'kill-buffer
  "b" 'switch-to-buffer
  "-" 'split-window-bellow
  "|" 'split-window-right)

; window navigation with vim-like bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;********************************************
; Vim plugins
;********************************************

; Vim Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

; Vim Commentary
(require 'evil-commentary)
(evil-commentary-mode)

; Evil-Matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)




;********************************************
; Dev settings
;********************************************

; show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
; FlyCheck linter
(add-hook 'after-init-hook #'global-flycheck-mode)

; indentation
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

; parentheses
(show-paren-mode t)

; enable autocomplation engine
(require 'auto-complete)
(global-auto-complete-mode t)

;********************************************
; Languages specific settings
;********************************************

; html
;; (require-package 'htmlize)


; Ruby mode
;; (require 'ruby)
; (add-to-list 'auto-mode-alist '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

; PHP mode
; (autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors flycheck smart-mode-line ## evil-leader evil-commentary evil-surround htmlize magit neotree evil json-mode web-server org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
