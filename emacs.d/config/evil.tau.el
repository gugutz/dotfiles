(require 'evil)
(evil-mode 1)

; (setq evil-esc-delay 0)

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

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq evil-default-cursor t)

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

;; (define-key evil-window-map "\C-h" 'evil-window-left)
;; (define-key evil-window-map "\C-j" 'evil-window-down)
;; (define-key evil-window-map "\C-k" 'evil-window-up)
;; (define-key evil-window-map "\C-l" 'evil-window-right)

(setq evil-emacs-state-cursor '("#ff0000" box))
(setq evil-motion-state-cursor '("#FFFFFF" box))
(setq evil-normal-state-cursor '("#00ff00" box))
(setq evil-visual-state-cursor '("#abcdef" box))
(setq evil-insert-state-cursor '("#e2f00f" bar))
(setq evil-replace-state-cursor '("red" hbar))
(setq evil-operator-state-cursor '("red" hollow))

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

;; (define-key evil-normal-state-map (kbd "C-n") 'mc/mark-next-like-this)
;; (define-key evil-normal-state-map (kbd "M-N") 'mc/mark-previous-like-this)

(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "q" 'evil-quit
  "w" 'save-buffer
  "k" 'kill-buffer
  "b" 'switch-to-buffer
  "-" 'split-windowellow
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

(require 'neotree)

(setq neo-window-width 30)

(global-set-key [f8] 'neotree-toggle)

(add-hook 'after-init-hook #'neotree-toggle)

(unless (display-graphic-p)
  (setq neo-theme 'icons))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq neo-smart-open t)

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

(require 'evil-commentary)
(evil-commentary-mode)

(evil-commentary-mode)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;(require 'evil-org)
(require 'evil-org-agenda)

(after 'org
  (require 'evil-org)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; (evil-org-agenda-set-keys)

(defvar my-leader-map (make-sparse-keymap)
   "Keymap for \"leader key\" shortcuts.")

 ;; binding "SPC" to the keymap
(define-key evil-normal-state-map (kbd "M-SPC") my-leader-map)

 ;; binding using SPC leader
 (define-key my-leader-map "b" 'list-buffers)
 (define-key my-leader-map "w" 'evil-save)
 (define-key my-leader-map "SPC" ":noh")

(require 'key-chord)
 (key-chord-mode 1)
 (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
 (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(provide 'evil.tau)
;;; evil.tau.el ends here...

(add-hook 'org-mode-hook                                                                      
          (lambda ()                                                                          
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 

;; (setq evil-want-C-i-jump nil)

(after 'magit
  (require 'evil-magit)
  (evil-magit-init))
