;; (setq user-full-name "Gustavo P Borges")
;; (setq user-mail-address "gugutz@gmail.com")

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
 '(TeX-after-compilation-finished-functions (quote TeX-revert-document-buffer) t)
 '(TeX-auto-save t t)
 '(TeX-master nil t)
 '(TeX-parse-self t t)
 '(TeX-view-program-list (quote (("pdf-tools" "TeX-pdf-tools-sync-view"))) t)
 '(TeX-view-program-selection (quote ((output-pdf "pdf-tools"))) t)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#3E4451")
 '(flycheck-display-errors-delay 1)
 '(jiralib-url "https://stairscreativestudio.atlassian.net" t)
 '(magit-auto-revert-mode nil)
 '(package-selected-packages
   (quote
    (pdf-tools ox-pandoc ox-reveal org-preview-html latex-preview-pane smart-mode-line-powerline-theme base16-theme gruvbox-theme darktooth-theme rainbow-mode smartscan restclient editorconfig prettier-js pandoc rjsx-mode js2-refactor web-mode evil-org multiple-cursors flycheck smart-mode-line ## evil-leader evil-commentary evil-surround htmlize magit neotree evil json-mode web-serverx org))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(font-lock-comment-face ((t (:foreground "#828282")))))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
;; (add-to-list 'load-path "~/dotfiles/emacs.d/config")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

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

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq linum-format " %d ")

(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

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

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

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

; parentheses
(show-paren-mode t)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)

(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode)
)

(use-package eldoc
  :diminish
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode)
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
(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
;; this replaces native capitlize word!
(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer

(use-package flyspell
  :defer 1
  :hook
  (text-mode . flyspell-mode)
)

(require 'epa-file)
(epa-file-enable)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
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
          yas-expand
          company-indent-or-complete-common
          emmet-expand-line
          )
  )
)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
;;  (undo-tree-mode)
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
  ("g t" . centaur-tabs-forward)
  ("g T" . centaur-tabs-backward)
  (", w" . evil-window-vsplit)
  ("C-r" . undo-tree-redo)
  )
  (:map evil-insert-state-map
  ;; this is also defined globally above in the config
  ("C-S-<tab>" . er/expand-region)
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

(use-package restclient
  :ensure t
  :mode "\\.rest$"
  :config
  (progn
    ;; Add hook to override C-c C-c in this mode to stay in window
    (add-hook 'restclient-mode-hook
              '(lambda ()
                 (local-set-key
                  (kbd "C-c C-c")
                  'restclient-http-send-current-stay-in-window))))
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
  :config
  (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)
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
  :config
  ;;(add-hook 'org-mode-hook
  ;;          (lambda ()
  ;;        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))
  ;; ox-extras
  ;; add suport for the ignore tag (ignores a headline without ignoring its content)
;;  (require ox-extra)
;;  (ox-extras-activate '(ignore-headlines))
  ;; general org config variables
  (setq org-log-done 'time)
  (setq org-export-backends (quote (ascii html icalendar latex md odt)))
  (setq org-use-speed-commands t)
  (setq org-confirm-babel-evaluate 'nil)
  (setq org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  (setq org-agenda-window-setup 'other-window)
  ;; Show CLOSED tag line in closed TODO items
  (setq org-log-done 'time)
  ;; Prompt to leave a note when closing an item
  (setq org-log-done 'note)

  ;;ox-twbs (exporter to twitter bootstrap html)
  (setq org-enable-bootstrap-support t)

  (defun org-export-turn-on-syntax-highlight()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'"
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
)

(use-package ox-extra
  :ensure nil
  :config
  (ox-extras-activate '(ignore-headlines))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
)

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

(use-package tex
  :ensure auctex
  :defer t
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
  (if (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode))
  (add-hook 'LaTeX-mode-hook
    (lambda ()
        (turn-on-reftex)
        (setq reftex-plug-into-AUCTeX t)
        (reftex-isearch-minor-mode)
        (setq TeX-PDF-mode t)
        (setq TeX-source-correlate-method 'synctex)
        (setq TeX-source-correlate-start-server t)))
)

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

(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (setq dired-dwin-target t)
  (add-hook 'dired-initial-position-hook #'dired-k)
)

(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status)
  ("C-x g" . magit-status)
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
  ;; (global-flycheck-inline-mode)
  (setq flycheck-inline-display-function
        (lambda (msg pos)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide))

(use-package treemacs
  :ensure t
  :defer t
  :hook
  (after-init . treemacs)
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("<f8>"       . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

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

(require 'neotree)

(global-set-key [f6] 'dired-sidebar-toggle-sidebar)

(setq dired-sidebar-subtree-line-prefix "__")
(setq dired-sidebar-theme 'vscode)
(setq dired-sidebar-use-term-integration t)
(setq dired-sidebar-use-custom-font t)

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

(use-package hl-line
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

(use-package parrot
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

(use-package centaur-tabs
   ;; :load-path "~/.emacs.d/other/centaur-tabs"
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
   :config
   (centaur-tabs-mode t)
   (setq centaur-tabs-style "bar") ; types available: (alternative, bar, box, chamfer, rounded, slang, wave, zigzag)
   (setq centaur-tabs-height 32)
   (setq centaur-tabs-set-icons t)
   (setq centaur-tabs-set-modified-marker t) ;; display a marker indicating that a buffer has been modified (atom-style)
   (setq centaur-tabs-modified-marker "*")
   (setq centaur-tabs-set-bar 'left) ;; in previous config value was 'over
   (setq centaur-tabs-gray-out-icons 'buffer)
   (setq centaur-tabs-headline-match)
   ;; (centaur-tabs-enable-buffer-reordering)
   ;; (setq centaur-tabs-adjust-buffer-order t)
   (setq uniquify-separator "/")
   (setq uniquify-buffer-name-style 'forward)
)

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
  (smartparens-enabled . evil-smartparens-mode)
)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (prog-mode . rainbow-delimiters-mode)
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

(smartscan-mode 1)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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

(use-package quickrun
  :bind
  ("C-<f5>" . quickrun)
  ("M-<f5>" . quickrun-shell)
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
  :config
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
)

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
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

(add-hook 'html-mode-hook
  (lambda ()
    (set (make-local-variable 'sgml-basic-offset) 4)))

(use-package yaml-mode
  :ensure t
)

(use-package toml-mode
  :ensure t
)

(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :init
  (setq css-indent-offset 2)
)

(use-package scss-mode
  :ensure t
  ;; this mode doenst load using :mode from use-package, dunno why
  ;; :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save 'nil)
  :config
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
)

(use-package helm-css-scss
  :ensure t
  :bind
  (:map isearch-mode-map
  ("s-i" . helm-css-scss-from-isearch)
  :map helm-css-scss-map
  ("s-i" . helm-css-scss-multi-from-helm-css-scss)
  :map css-mode-map
  ("s-i" . helm-css-scss)
  ("s-I" . helm-css-scss-back-to-last-point)
  :map scss-mode-map
  ("s-i" . helm-css-scss)
  ("s-I" . helm-css-scss-back-to-last-point)
  )
  :config
  (setq helm-css-scss-insert-close-comment-depth 2
        helm-css-scss-split-with-multiple-windows t
        helm-css-scss-split-direction 'split-window-vertically)
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
)

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode
  ("\\.js$" . js2-mode)

  :hook
  (js2-mode . flycheck-mode)
  (js2-mode . company-mode)
  (js2-mode . lsp-mode)
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
  )
)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-inhibit-message t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-highlight-symbol-at-point nil)
  :hook
  (js2-mode . lsp)
  (js2-jsx-mode . lsp)
  (typescript-mode . lsp)
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
  :hook ((lsp-mode . lsp-ui-mode))
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
