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

(use-package diminish :ensure t)


;; **************************************************
;;
;;
;;; Emacs settings

;; ----------------------------------
;;; General

(blink-cursor-mode 0) ;; dont blink the cursor

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

(setq kill-do-not-save-duplicates t) ;; Eliminate duplicates in the kill ring.

(setq next-line-add-newlines t) ;; C-n insert newlines if the point is at the end of the buffer.

;; ----------------------------------
;;; Startup

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t)
(setq  inhibit-startup-echo-area-message user-login-name)
(setq  inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)
(setq  initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; ----------------------------------
;;; Files
(setq create-lockfiles nil) ;; Prevent emacs to create lockfiles (.#files#). this also stops preventing editing colisions, so watch out
(setq make-backup-files nil) ;; dont make backup files
(setq confirm-kill-processes nil) ;; dont ask confirmation to kill processes


;; ----------------------------------
;;; Minibuffer settings

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; **************************************************
;;
;;; Evil

(use-package evil
  :ensure t
  :bind
  ;;;;;;; FOR SOME REASON WHEN I MAP KEYS HERE IN :bind EVIL DOENST LOAD ON STARTUP
  ;;;;
  ;; (:map evil-normal-state-map
  ;;   ("SPC q" . evil-quit)
  ;;   ;; ("SPC q" . kill-buffer-and-window) ;; check if evil quit does this
  ;;   ("SPC w" . save-buffer)
  ;;   ("SPC d" . delete-frame)
  ;;   ("SPC k" . kill-buffer)
  ;;   ("SPC -" . split-window-bellow)
  ;;   ("SPC |" . split-window-right)
  ;;   ("SPC u" . undo-tree-visualize))
  ;; (:map evil-insert-state-map
  ;;   ("C-a" .  move-beginning-of-line)
  ;;   ("C-e" .  move-end-of-line)
  ;;   ("C-y" .  yank)
  ;;   ("C-n" .  evil-next-line)
  ;;   ("C-p" .  evil-previous-line))
  ;; (:map evil-motion-state-map
  ;;   ("C-e" .  evil-end-of-line))
  :custom
  ;; change cursor color according to mode
  (evil-emacs-state-cursor '("#ff0000" box))
  (evil-motion-state-cursor '("#FFFFFF" box))
  (evil-normal-state-cursor '("#00ff00" box))
  (evil-visual-state-cursor '("#abcdef" box))
  (evil-insert-state-cursor '("#e2f00f" bar))
  (evil-replace-state-cursor '("red" hbar))
  (evil-operator-state-cursor '("red" hollow))
  ;; evil in modeline
  (evil-mode-line-format '(before . mode-line-front-space)) ;; move evil tag to beginning of modeline
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
  ;; ("SPC q" . kill-buffer-and-window) ;; check if evil quit does this
  (define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC d") 'delete-frame)
  (define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
  (define-key evil-normal-state-map (kbd "SPC -") 'split-window-bellow)
  (define-key evil-normal-state-map (kbd "SPC |") 'split-window-right)
  (define-key evil-normal-state-map (kbd "SPC u") 'undo-tree-visualize)
  )

;; ---------------------------------
;; evil-commentary

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish
  :config
  (evil-commentary-mode)
  )


;; **************************************************
;;
;;; Ivy

(use-package ivy
  :ensure t
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
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
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
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC b") 'counsel-switch-buffer)
    (define-key evil-normal-state-map (kbd "SPC e") 'counsel-find-file)
    (define-key evil-normal-state-map (kbd "SPC f") 'counsel-projectile-find-file)
    (define-key evil-normal-state-map (kbd "SPC a g") 'counsel-ag)
    (define-key evil-normal-state-map (kbd "SPC r g") 'counsel-rg)
    )
  )

;; ---------------------------------
;;
;;
;;; Counsel
(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  )

;; ---------------------------------
;;
;;
;; Enhance M-x
;; Enhancement for `execute-extended-command'. Auto detects and uses ivy or ido, if installed
(use-package amx
  :ensure t
  :after ivy
  :config
  (amx-mode)
  )

;; ---------------------------------
;;
;; ivy-rich

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

;; ---------------------------------
;;
;; ** ivy-prescient
(use-package ivy-prescient
  :ensure t
  :after ivy
  :config
  (ivy-prescient-mode)
  )

;; ---------------------------------
;;
;; all the icons for ivy

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  )

;; ---------------------------------
;;
;;
;;; avy
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

;; ---------------------------------
;;
;; goto-line-preview

(use-package goto-line-preview
  :ensure t
  ;; :config
  ;; (global-set-key [remap goto-line] 'goto-line-preview)
  )


;; **************************************************
;;
;;; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("<f8>" . treemacs)
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

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; **************************************************
;;
;;; UI Settings

;; Add `./themes' to path
(add-to-list 'custom-theme-load-path "~/dotfiles/emacs.d/themes/")

;; Set the used theme
(setq my-theme 'vscode-default-dark)

;;  Load the theme
(load-theme my-theme t)

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

;; scroll bars from frames
(scroll-bar-mode -1)

;; Remove menu bar and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; --------------------------------------
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
  )

;; ---------------------------------
;;
;; vi tilde fringe

;; Displays tildes in the fringe on empty lines a la Vi.

(use-package vi-tilde-fringe
  :ensure t
  :diminish
  :config
  (global-vi-tilde-fringe-mode)
  )

;; ---------------------------------
;;
;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)
;; Set the whitespace highlight color
(set-face-background 'trailing-whitespace "#f44545")

;; **************************************************
;;
;;; WINdow Management

;; auto balance windows on opening and closing frames
(setq window-combination-resize t)

;; ** auto balance windows area
(global-set-key (kbd "C-M-+") 'balance-windows-area)

;; ---------------------------------
;;
;; ** Eyebrowse

(use-package eyebrowse
  :ensure t
  :bind
  (:map eyebrowse-mode-map
    ("M-1" . eyebrowse-switch-to-window-config-1)
    ("M-2" . eyebrowse-switch-to-window-config-2)
    ("M-3" . eyebrowse-switch-to-window-config-3)
    ("M-4" . eyebrowse-switch-to-window-config-4)
    ("H-<right>" . eyebrowse-next-window-config)
    ("H-<left>" . eyebrowse-prev-window-config))
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t)
  )

;; ---------------------------------
;;
;; Ace-Window

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :config
  (setq aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; set the window labels in the home row
  )

;; ---------------------------------
;;
;; emacsrotate

(use-package rotate
  :ensure t
  :defer t
  :bind
  ("C-c r w" . rotate-window)
  ("C-c r l" . rotate-layout)
  ("M-S-O SPC" . rotate-layout)
  ("M-S-O m" . tau/toggle-window-maximize)
  ("C-M-o" . hydra-frame-window/body)
  )



;; ---------------------------------
;;
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

;; **************************************************
;;
;;; ** Projectile

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
  )

;; **************************************************
;;
;;; ** Company

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :bind
    (:map company-active-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next)
    ("<tab>" . company-complete-common-or-cycle)
    ("<backtab>" . company-select-previous))
    (:map company-search-map
    ("C-p" . company-select-previous)
    ("C-n" . company-select-next))
    
  :custom
  (company-idle-delay 0.1) ;; decrease delay before autocompletion popup shows
  (company-echo-delay 0) ;; remove annoying blinking
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; show tooltip even for single candidates
  (setq company-frontends '(company-pseudo-tooltip-frontend
                             company-echo-metadata-frontend))
  )

;; ---------------------------------
;;
;;; company box

(use-package company-box
  :ensure t
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

;; **************************************************
;;
;; ** LSP

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

;; ---------------------------------
;;
;; company-lsp

;; Company-lsp is auto inserted into company backends

(use-package company-lsp :ensure t
  :ensure t
  ;; :custom
  ;; (company-lsp-enable-snippet t)
  ;; (company-lsp-async t)
  ;; (company-lsp-cache-candidates 'auto)
  ;; (company-lsp-enable-recompletion t)
  )

;; **************************************************
;;
;;; ** Yasnippets

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

;; ---------------------------------
;;
;; Colection of snippets
(use-package yasnippet-snippets :ensure t)


;; **************************************************
;;
;; dumb-jump

(use-package dumb-jump
  :ensure t
  :after ivy
  :bind
    ("C-c C-o" . dumb-jump-go-other-window)
    ("C-c C-j" . dumb-jump-go)
    ("C-c C-b" . dumb-jump-back)
    ("C-c C-i" . dumb-jump-go-prompt)
  :custom
  (dumb-jump-selector 'ivy)
  )

;; **************************************************
;;
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

;; **************************************************
;;
;; ** helpfull

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
;;
;; LANGUAGES SETUP


;; ---------------------------------
;;
;; Typescript Mode

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode)
    ("\\.tsx\\'" . typescript-mode))
  )

;; ---------------------------------
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
;;
;; SIMULATE VSCODE FUNCTIONS

;; ---------------------------------
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

;; ---------------------------------
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

(global-set-key (kbd "C-S-j") 'move-line-down)

(defun move-line-up ()
  "Move line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-k") 'move-line-up)

;; ---------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-delay 0 t)
 '(company-idle-delay 0.1)
 '(company-lsp-cache-candidates (quote auto))
 '(dumb-jump-selector (quote ivy) t)
 '(evil-emacs-state-cursor (quote ("#ff0000" box)) t)
 '(evil-insert-state-cursor (quote ("#e2f00f" bar)) t)
 '(evil-mode-line-format (quote (before . mode-line-front-space)))
 '(evil-motion-state-cursor (quote ("#FFFFFF" box)) t)
 '(evil-normal-state-cursor (quote ("#00ff00" box)) t)
 '(evil-operator-state-cursor (quote ("red" hollow)) t)
 '(evil-replace-state-cursor (quote ("red" hbar)) t)
 '(evil-visual-state-cursor (quote ("#abcdef" box)) t)
 '(eyebrowse-mode nil)
 '(global-company-mode t)
 '(hydra-posframe-parameters (quote ((left-fringe . 5) (right-fringe . 5))) t)
 '(lsp-prefer-flymake nil)
 '(package-selected-packages
   (quote
    (ivy-explorer yasnippet-snippets yasnippet company-box all-the-icons-ivy ivy-prescient ivy-rich amx shell-pop typescript-mode company-lsp goto-line-preview lsp-mode eyebrowse vi-tilde-fringe helpful dumb-jump company all-the-icons rotate treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs evil-leader evil-commentary diminish ivy evil use-package)))
 '(projectile-completion-system (quote ivy) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(hydra-posframe-border-face ((t (:background "#6272a4")))))
