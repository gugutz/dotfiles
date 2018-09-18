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

(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

(setq create-lockfiles nil)

(setq vc-follow-symlinks t)

(when (eq window-system nil)
  (xterm-mouse-mode 1))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
    `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
    `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
    emacs-tmp-dir)

(setq ring-bell-function 'ignore)

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

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;(after 'evil
;  (require 'evil.tau))

  (require 'evil.tau)

;(after 'org
;  (require 'org.tau))

  (require 'org.tau)

(defun /eshell/new-window ()
    "Opens up a new shell in the directory associated with the current buffer's file.  The eshell is renamed to match that directory to make multiple eshell windows easier."
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

(require 'helm)

(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 40)
(setq helm-split-window-inside-p t)
(setq helm-mode-fuzzy-match t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-ff-skip-boring-files t)
(setq helm-follow-mode-persistent t)

(after 'helm-source
  (defun /helm/make-source (f &rest args)
    (let ((source-type (cadr args))
          (props (cddr args)))
      (unless (child-of-class-p source-type 'helm-source-async)
        (plist-put props :fuzzy-match t))
      (apply f args)))
  (advice-add 'helm-make-source :around '/helm/make-source))

(after 'helm
  ;; take between 10-30% of screen space
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 30)
  (helm-autoresize-mode t))

(progn
(global-set-key [remap execute-extended-command] #'helm-M-x)
(global-set-key [remap find-file] #'helm-find-files)
(helm-mode t))

(after 'helm
  (require 'helm-config)
  (global-set-key (kbd "C-c h") #'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-h a") #'helm-apropos)
  (global-set-key (kbd "C-x b") #'helm-buffers-list)
  (global-set-key (kbd "C-x C-b") #'helm-mini)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x r b") #'helm-bookmarks)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "M-:") #'helm-eval-expression-with-eldoc)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
)

(after 'dired
  (require 'dired-k)
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (add-hook 'dired-initial-position-hook #'dired-k))

(global-set-key (kbd "C-x g") 'magit-status)

(require 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(setq which-key-max-description-length 20)
(setq which-key-max-display-columns 6)
(which-key-mode)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq linum-format " %d ")

(setq x-select-enable-clipboard t)

;; for some readon the bellow lines should be the default native way for navigation on emacs
;; but they dont work
;; using the above package instead til i find a solution
;
;; (windmove-default-keybindings 'control)
;; (global-set-key (kbd "C-h") 'windmove-left)
;; (global-set-key (kbd "C-l") 'windmove-right)
;; (global-set-key (kbd "C-k") 'windmove-up)
;; (global-set-key (kbd "C-j") 'windmove-down)

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
;;   ("C-w h" #'evil-window-left)
;;   ("C-w j" #'evil-window-down)
;;   ("C-w k" #'evil-window-up)
;;   ("C-w l" #'evil-window-right))

;; (/bindings/define-keys evil-normal-state-map
;;   ("C-w h" #'evil-window-left)
;;   ("C-w j" #'evil-window-down)
;;   ("C-w k" #'evil-window-up)
;;   ("C-w l" #'evil-window-right))

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-_") #'text-scale-decrease)
;; (global-set-key (kbd "C-)") #'text-scale-adjust)

(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

(setq next-line-add-newlines t)

(smartscan-mode 1)

(pdf-tools-install)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode) 
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)

(evil-define-key 'normal pdf-view-mode-map
  "h" 'pdf-view-previous-page-command
  "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
  "l" 'pdf-view-next-page-command)

; parentheses
(show-paren-mode t)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)

(add-hook 'org-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)

(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")
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

(require 'prettier-js)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.js?\\'" . prettier-js-mode)
                             '("\\.jsx?\\'" . prettier-js-mode)
                             '("\\.css?\\'" . prettier-js-mode))))

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(js2r-add-keybindings-with-prefix "C-c C-m")

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'company)
(global-company-mode t)
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

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'after-init-hook 'global-company-mode)

; (after "company-autoloads"
;    (define-key evil-insert-state-map (kbd "TAB")
;      #'company-indent-or-complete-common))

;; (add-to-list 'load-path
;;               "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(setq emmet-expand-jsx-className? t) ;; default nil

;(require 'ruby.tau)
;(add-to-list 'auto-mode-alist '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

; (autoload 'go-mode "go-mode" "Major mode for editing Go code." t)
; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

; (autoload 'mardown-mode "markdown-mode")
; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; ; (require 'haskell-interactive-mode)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (eval-after-load 'flycheck
;                  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
; (add-hook 'haskell-mode-hook (lambda ()
;                                (electric-indent-mode -1)))
; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
; (add-hook 'haskell-mode-hook (lambda () (global-set-key (kbd "<f5>") 'haskell-process-cabal-build)))

(require 'ox-latex)

(require 'tex)

(setq exec-path (append exec-path '("/usr/bin/tex")))

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-to-list 'org-latex-classes
	     '("beamer"
	       "\\documentclass\[presentation\]\{beamer\}"
	       ("\\section\{%s\}" . "\\section*\{%s\}")
	       ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
	       ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(add-to-list 'org-latex-classes
             '("memoir"
               "\\documentclass\[a4paper\]\{memoir\}"
               ("\\book\{%s\}" . "\\book*\{%s\}")
               ("\\part\{%s\}" . "\\part*\{%s\}")
               ("\\chapter\{%s\}" . "\\chapter*\{%s\}")
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(add-to-list 'org-latex-classes
          '("abntex2"
            "\\documentclass{abntex2}"
            ("\\part{%s}" . "\\part*{%s}")
            ("\\chapter{%s}" . "\\chapter*{%s}")
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}"))
          )

(latex-preview-pane-enable)

(setq TeX-PDF-mode t)

(TeX-global-PDF-mode t)

(font-lock-add-keywords
   'latex-mode
   `((,(concat "^\\s-*\\\\\\("
               "\\(documentclass\\|\\(sub\\)?section[*]?\\)"
               "\\(\\[[^]% \t\n]*\\]\\)?{[-[:alnum:]_ ]+"
               "\\|"
               "\\(begin\\|end\\){document"
               "\\)}.*\n?")
      (0 'your-face append))))

(setq org-export-with-smart-quotes t)

(setq org-latex-remove-logfiles nil)

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

(defun my-save ()
  "Save file when leaving insert mode in Evil."
  (if (buffer-file-name)
      (evil-save)))

(add-hook 'evil-insert-state-exit-hook 'my-save)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:


(provide 'init)
;;; .emacs ends here
