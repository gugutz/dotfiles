;; (require 'expand-region)
(global-set-key (kbd "C-TAB") 'er/expand-region)

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :bind
  (:map evil-normal-state-map
      ("g t" . centaur-tabs-forward)
      ("g T" . centaur-tabs-backward)))
  :config ;; tweak evil after loading it
  (evil-mode)
  ;; Don't wait for any other keys after escape is pressed.
  (setq evil-esc-delay 0)
  ;; example how to map a command in normal mode (called 'normal state' in evil)
  ;; Make Evil look a bit more like (n) vim  (??)
  (setq evil-search-module 'isearch-regexp)
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
  ;; move evil tag to beginning of modeline
  (setq evil-mode-line-format '(before . mode-line-front-space))

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
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-normal-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-w") 'kill-region)
(define-key evil-normal-state-map (kbd "C-w") 'kill-region)
(define-key evil-visual-state-map (kbd "C-w") 'kill-region)
(define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)
(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-r" 'search-backward)

(define-key evil-insert-state-map (kbd "C-TAB") 'er/expand-region)

;; (define-key evil-window-map "\C-h" 'evil-window-left)
;; (define-key evil-window-map "\C-j" 'evil-window-down)
;; (define-key evil-window-map "\C-k" 'evil-window-up)
;; (define-key evil-window-map "\C-l" 'evil-window-right)

(require 'evil-leader)

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

(require 'evil-surround)
(global-evil-surround-mode 1)

(defun evil-surround-prog-mode-hook-setup ()
  "Documentation string, idk, put something here later."
  (push '(47 . ("/" . "/")) evil-surround-pairs-alist)
  (push '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(41 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist))
(add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)

(defun evil-surround-js-mode-hook-setup ()
  "ES6." ;  this is a documentation string, a feature in Lisp
  ;; I believe this is for auto closing pairs
  (push '(?1 . ("{`" . "`}")) evil-surround-pairs-alist)
  (push '(?2 . ("${" . "}")) evil-surround-pairs-alist)
  (push '(?4 . ("(e) => " . "(e)")) evil-surround-pairs-alist)
  ;; ReactJS
  (push '(?3 . ("classNames(" . ")")) evil-surround-pairs-alist)
  (add-hook 'js2-mode-hook 'evil-surround-js-mode-hook-setup))

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)
(defun evil-surround-org-mode-hook-setup ()
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status)
  ("C-x g" . magit-status))

(use-package evil-magit
  :init
  (evil-magit-init)
  (setq evil-magit-state 'normal)
  (setq evil-magit-use-y-for-yank nil)
  :config
  (evil-define-key evil-magit-state magit-mode-map "j" 'magit-log-popup)
  (evil-define-key evil-magit-state magit-mode-map "k" 'evil-next-visual-line)
  (evil-define-key evil-magit-state magit-mode-map "l" 'evil-previous-visual-line))

(use-package treemacs
  :ensure t
  :defer t
  :hook (after-init . treemacs)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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
  :config
  (global-set-key [f7] 'neotree-toggle)
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
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
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

(provide 'evil.tau)
;;; evil.tau.el ends here...
