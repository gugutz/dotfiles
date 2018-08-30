;;; package -- Summary
;############################################
;; evil

;############################################
;;; Commentary:

;; these are the bindings i eventually got to
;; while trying to resolve conflicts
;; with evil disabling native Emacs bindings

;############################################
;;; Code:


; require Evil related packages
(require 'evil)
(require 'neotree)
(require 'evil-leader)
(require 'evil-surround)
(require 'evil-commentary)
(require 'evil-matchit)
(require 'evil-org)
(require 'evil-org-agenda)


(evil-mode 1)
;********************************************
;; Simulate Vim behaviour and some bindings
;********************************************

;; make esc quit or cancel everything in Emacs
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; @see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default
;; Cursor is alway black because of evil.
;; Here is the workaround
(setq evil-default-cursor t)


;; recover native emacs commands that are overriden by evil
;; this gives priority to native emacs behaviour rathen than Vim's
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


; Pull eshell in a new bottom window
(define-key evil-normal-state-map (kbd "!") #'/eshell/new-window)
(define-key evil-visual-state-map (kbd "!") #'/eshell/new-window)
(define-key evil-motion-state-map (kbd "!") #'/eshell/new-window)

;********************************************
; multiple cursors
;********************************************

;; step 1, select thing in visual-mode (OPTIONAL)
;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
;; step 4, press s or S to start replace
;; step 5, press C-g to quit multiple-cursors
(define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
(define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
(define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
(define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor)

; imitate vim multiple selection behavior with multiple-cursors package
;; (define-key evil-normal-state-map (kbd "C-n") 'mc/mark-next-like-this)
;; (define-key evil-normal-state-map (kbd "M-N") 'mc/mark-previous-like-this)



;********************************************
; evil-leader
;********************************************

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

;********************************************
;; Evil Surround
;; {{ @see https://github.com/timcharper/evil-surround for tutorial
;********************************************

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
  "ES6." ;  <-- this is a documentation string, a feature in Lisp
  ;; I believe this is for auto closing pairs
  (push '(?1 . ("{`" . "`}")) evil-surround-pairs-alist)
  (push '(?2 . ("${" . "}")) evil-surround-pairs-alist)
  (push '(?4 . ("(e) => " . "(e)")) evil-surround-pairs-alist)
  ;; ReactJS
  (push '(?3 . ("classNames(" . ")")) evil-surround-pairs-alist))
(add-hook 'js2-mode-hook 'evil-surround-js-mode-hook-setup)

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)
(defun evil-surround-org-mode-hook-setup ()
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)


;********************************************
; Neotree
;********************************************

;; set NeoTree default window width
(setq neo-window-width 29)

;; use neotree (NERDTree for emacs) and toggle with F8
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
            (define-key evil-normal-state-local-map (kbd "|") 'neotree-enter-vertical-split)
            (define-key evil-normal-state-local-map (kbd "-") 'neotree-enter-horizontal-split)
            ; simulating NERDTree bindings in Neotree
            (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "u") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "C") 'neotree-change-root)
            (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)))


;********************************************
; Vim plugins definitions
;********************************************

;  To enable evil-commentary permanently, add
(evil-commentary-mode)

; Vim Commentary
(evil-commentary-mode)

; Evil-Matchit
(global-evil-matchit-mode 1)

;********************************************
; Evil-ORG for ORG mode compatibility
;********************************************

(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-org-agenda-set-keys)


;############################################
(provide 'evil.tau)
;;; evil.tau.el ends here

