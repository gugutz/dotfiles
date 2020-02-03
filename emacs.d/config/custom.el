(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-delay 0 t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-margin 2)
 '(dumb-jump-selector (quote ivy) t)
 '(evil-emacs-state-cursor (quote ("#ff0000" box)) t)
 '(evil-insert-state-cursor (quote ("#e2f00f" bar)) t)
 '(evil-mode-line-format (quote (before . mode-line-front-space)))
 '(evil-motion-state-cursor (quote ("#FFFFFF" box)) t)
 '(evil-normal-state-cursor (quote ("#00ff00" box)) t)
 '(evil-operator-state-cursor (quote ("#ff0000" hollow)) t)
 '(evil-replace-state-cursor (quote ("#ff0000" hbar)) t)
 '(evil-visual-state-cursor (quote ("#abcdef" box)) t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled newline)))
 '(flycheck-display-errors-delay 1)
 '(flycheck-idle-change-delay 1.5)
 '(flycheck-indication-mode (quote left-fringe))
 '(flycheck-posframe-border-width 2)
 '(flycheck-posframe-error-prefix "❌ ")
 '(flycheck-posframe-info-prefix " ")
 '(flycheck-posframe-prefix "➤ ")
 '(flycheck-posframe-warning-prefix "⚠ ")
  '(package-selected-packages
     (quote
       (parrot minions anzu highlight-indent-guides rainbow-mode hl-todo diff-hl highlight-escape-sequences highlight-operators highlight-numbers rainbow-delimiters highlight-parentheses solaire-mode vi-tilde-fringe centaur-tabs yaml-mode emmet-mode prettier-js scss-mode web-mode typescript-mode aggressive-indent editorconfig shell-pop dumb-jump keyfreq which-key helpful flycheck-posframe flycheck yasnippet-snippets use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil smartparens rotate nyan-mode lsp-ui ivy-rich ivy-prescient ivy-posframe ivy-explorer goto-line-preview eyebrowse expand-region exec-path-from-shell evil-commentary drag-stuff dired-k diminish counsel-projectile company-quickhelp company-lsp company-box bug-hunter amx all-the-icons-ivy all-the-icons-dired)))
 '(prettier-js-show-errors (quote buffer) t)
 '(web-mode-enable-block-face t t)
 '(web-mode-enable-current-element-highlight t t)
 '(web-mode-enable-part-face t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((nil (:foreground "yellow" :weight (quote bold)))))
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(diff-hl-change ((t (:foreground "#f4f4f4"))))
 '(diff-hl-delete ((t (:background "#ff3300" :foreground "#ffffff"))))
 '(diff-hl-insert ((t (:background "#00ff00" :foreground "#000000"))))
 '(flycheck-fringe-error ((nil (:background "#ff3300" :foreground "#000000"))))
 '(flycheck-fringe-info ((nil (:background "#007ad3" :foreground "#ffffff"))))
 '(flycheck-fringe-warning ((nil (:background "#fcfa23" :foreground "#000000"))))
 '(flycheck-posframe-border-face ((nil (:foreground "#288aaa" :background "#9370DB"))))
 '(flycheck-posframe-error-face ((nil (:background "#000000" :foreground "#ff3300" :height 105))))
 '(flycheck-posframe-info-face ((nil (:background "#007ad3" :foreground "#ffffff" :height 105))))
 '(flycheck-posframe-warning-face ((nil (:background "#fcfa23" :foreground "#000000" :height 105))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(ivy-posframe ((t (:background "#202020"))))
 '(ivy-posframe-border ((t (:background "#9370DB"))))
 '(ivy-posframe-cursor ((t (:background "#00ff00"))))
 '(show-paren-match ((nil (:background "#9370DB" :foreground "#ffffff"))))
 '(show-paren-mismatch ((nil (:background "red" :foreground "black")))))
