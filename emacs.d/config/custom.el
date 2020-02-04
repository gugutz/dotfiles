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
 '(flycheck-posframe-border-width 1)
 '(flycheck-posframe-error-prefix "❌ ")
 '(flycheck-posframe-info-prefix " ")
 '(flycheck-posframe-prefix "➤ ")
 '(flycheck-posframe-warning-prefix "⚠ ")
  '(package-selected-packages
     (quote
       (git-timemachine vc-msg yasnippet-snippets yaml-mode which-key web-mode vimrc-mode vi-tilde-fringe use-package typescript-mode try treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil solaire-mode smartparens shell-pop scss-mode rotate restart-emacs rainbow-mode rainbow-delimiters prettier-js parrot nyan-mode minions lsp-ui keyfreq ivy-rich ivy-prescient ivy-posframe ivy-explorer hl-todo highlight-parentheses highlight-operators highlight-numbers highlight-indent-guides highlight-escape-sequences helpful goto-line-preview flycheck-posframe eyebrowse expand-region exec-path-from-shell evil-matchit evil-commentary emmet-mode editorconfig dumb-jump drag-stuff dired-k diminish diff-hl counsel-projectile company-quickhelp company-lsp company-box centaur-tabs anzu amx all-the-icons-ivy all-the-icons-dired aggressive-indent)))
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
 '(diff-hl-change ((t (:foreground nil))))
 '(diff-hl-delete ((t (:background "#ff3300" :foreground "#ffffff"))))
 '(diff-hl-insert ((t (:background "#00ff00" :foreground "#000000"))))
 '(flycheck-fringe-error ((nil (:background "#ff3300" :foreground "#000000"))))
 '(flycheck-fringe-info ((nil (:background "#007ad3" :foreground "#ffffff"))))
 '(flycheck-fringe-warning ((nil (:background "#fcfa23" :foreground "#000000"))))
 '(flycheck-posframe-error-face ((nil (:background "#000000" :foreground "#ff3300" :height 105))))
 '(flycheck-posframe-info-face ((nil (:background "#007ad3" :foreground "#ffffff" :height 105))))
 '(flycheck-posframe-warning-face ((nil (:background "#fcfa23" :foreground "#000000" :height 105))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(ivy-posframe ((t (:background "#202020"))))
 '(ivy-posframe-border ((t (:background "#9370DB"))))
 '(ivy-posframe-cursor ((t (:background "#00ff00"))))
 '(show-paren-match ((nil (:background "#9370DB" :foreground "#ffffff"))))
 '(show-paren-mismatch ((nil (:background "red" :foreground "black")))))
