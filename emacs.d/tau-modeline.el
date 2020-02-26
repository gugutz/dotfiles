
;;; tau modeline.el

;; Copyright (C) 2019 Gustavo P Borges

;; Maintainer: gugutz@gmail.com
;; Package: tau-modeline
;; Homepage:
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
;; Keywords: internal

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO

;;; Code:


;; **************************************
;; Modified or Read Only


;; This snippet displays a chain icon when the current file is saved, a broken chain when it is modified and a pad lock when the file is read only.

;; (defun custom-modeline-modified
;;   ((let* ((config-alist
;;             '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
;;               ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
;;               ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
;;            (result (cdr (assoc (format-mode-line "%*") config-alist))))
;;       (propertize (apply (cadr result) (cddr result))
;;                   'face `(:family ,(funcall (car result)))))))


;; **************************************
;; Mode Icon

(defun custom-modeline-mode-icon ()
  "Display the major mode icon."
  (format " %s"
    (propertize icon
      'help-echo (format "Major-mode: `%s`" major-mode)
      'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))


;; *********************************
;;; Region Marking

(defun custom-modeline-region-info ()
  "This snippet displays useful information on the current marked region, i.e. number of lines and characters marked."
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
           (chars (count-words (region-end) (region-beginning))))
      (concat
        (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
          'face `(:family ,(all-the-icons-octicon-family))
          'display '(raise -0.0))
        (propertize (format " (%s, %s)" words chars)
          'face `(:height 0.9))))))

;; *********************************
;;; Version control icons

(defun -custom-modeline-github-vc ()
  "Custom icon for git / github repositories."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
      (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
      " · "
      (propertize (format "%s" (all-the-icons-octicon "git-branch"))
        'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
        'display '(raise -0.1))
      (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  "Custom icon for svn repositories."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
      (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
      (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  "Display a vc icon according to the type of vc system (git || svn)."
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))

;; *********************************
;;; Flycheck icons for modeline

(defun custom-modeline-flycheck-status ()
  "Display flycheck report on the modeline."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                              (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                             (+ (or .warning 0) (or .error 0)))))
                                (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
                              "✔ No Issues"))
                 (`running     "⟲ Running")
                 (`no-checker  "⚠ No Checker")
                 (`not-checked "✖ Disabled")
                 (`errored     "⚠ Error")
                 (`interrupted "⛔ Interrupted")
                 (`suspicious  ""))))
    (propertize text
      'help-echo "Show Flycheck Errors"
      'mouse-face '(:box 1)
      'local-map (make-mode-line-mouse-map
                   'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

;; *********************************
;;; Time with clock icon

(defun custom-modeline-time ()
  "Display a clock icon with the current time."
  (let* ((hour (string-to-number (format-time-string "%I")))
          (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
    (concat
      (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
      (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

;; *********************************
;;; LSP info in the modelin

(defun custom-lsp-mode-line ()
  "Construct the mode line text."
  (if-let (workspaces (lsp-workspaces))
    (concat "LSP" (string-join (--map (format "[%s]" (lsp--workspace-print it))
                                 workspaces)))
    (concat "LSP" (propertize "[Disconnected]" 'face 'warning))))


;; ;; *********************************
;; ;;
;; ;;; My personal modeline

(set-face-attribute 'mode-line nil
  :background "#353644"
  :foreground "white"
  :box '(:line-width 5 :color "#353644")
  :overline nil
  :underline nil)

(set-face-attribute 'mode-line-inactive nil
  :background "#565063"
  :foreground "white"
  :box '(:line-width 5 :color "#565063")
  :overline nil
  :underline nil)

;;;###autoload
(define-minor-mode tau-modeline
  "My customizations on the modeline"
  :lighter " TAU MODELINE"
  :global t
  (if gcmh-mode
    (progn

      (setq-default mode-line-format
        (list
          ;; evil status
          evil-mode-line-tag
          '(:eval (propertize (if (eq 'emacs evil-state) "  " "  ")
                    'face font-lock-builtin-face))


          ;; major mode icon
          "%2 "
          "|"
          '(:eval (custom-modeline-mode-icon))

          ;; Buffer modified status
          "%2 "
          ;; '(:eval   (custom-modeline-modified))

          ;; Display a broken chain if buffer is modified
          '(:eval
             (when (eql (buffer-modified-p) t)
               ;; propertize adds metadata to text, so you can add colours and formatting, amongst other things
               (propertize (all-the-icons-faicon "chain-broken")
                 'face `(:family ,(all-the-icons-faicon-family)
                          :height 1.2))))
          ;; Display a lock if buffer is readonly
          '(:eval
             (when (eql buffer-read-only t)
               (propertize (all-the-icons-faicon "lock")
                 'face `(:family ,(all-the-icons-faicon-family)
                          :height 1.2))))

          ;; the buffer name; the file name as a tool tip
          ;; if buffer is modified, change the background and foreground colors
          '(:eval (propertize "%b "
                    'face
                    (let ((face (buffer-modified-p)))
                      (if face 'font-lock-warning-face
                        'font-lock-type-face))
                    'help-echo (buffer-file-name))
             (when (buffer-modified-p)
               (propertize " " 'face 'font-lock-warning-face))

             (when buffer-read-only
               (propertize "" 'face 'font-lock-warning-face))
             )

          "("
          (propertize "%02I" 'face 'font-lock-constant-face) ;; size of file
          ")"

          ;; line and column
          " "
          (propertize "%02l" 'face 'font-lock-keyword-face) ;; line number
          ":"
          (propertize "%c" 'face 'font-lock-keyword-face) ;; column number
          " "
          (propertize "%p" 'face 'font-lock-constant-face) ;; % of buffer above top of window


          ;; marked region info (lines selected, word count)
          '(:eval   (custom-modeline-region-info))


          ;; projectile
          " | "
          '(:eval (when (bound-and-true-p projectile-mode)
                    (projectile-mode-line-function)))

          ;; nyan cat
          ;; "%2 "
          ;; '(:eval (when (bound-and-true-p nyan-mode)
          ;;           (nyan-create)))


          ;; party parrot
          ;; "%2 "
          ;; '(:eval (when (bound-and-true-p parrot-mode)
          ;;           (parrot-create)))

          ;; ;; spaces to align right
          ;; '(:eval (propertize
          ;;           " " 'display
          ;;           `((space :align-to (- (+ right right-fringe right-margin)
          ;;                                ,(+ 60 (string-width mode-name))))))) ;; the bigger the number, less space is added



          ;; ;; spaces to align right
          ;; '(:eval (propertize
          ;; " " 'display
          ;; `((space :align-to (- (+ right right-fringe right-margin)
          ;; 			,(+ (string-width org-mode-line-string) (+ 3 (string-width mode-name)))
          ;; 			)))))


          ;; the current major mode
          ;; '(:eval   (custom-modeline-mode-icon))
          (propertize "%m" 'face 'font-lock-string-face)

          ;; lsp status
          '(:eval (when (bound-and-true-p lsp-mode)
                    " | "
                    (custom-lsp-mode-line)))

          ;; day and time
          ;; '(:eval (propertize (format-time-string " %b %d %H:%M ")
          ;; 			'face 'font-lock-builtin-face))
          "|"
          '(:eval
             (custom-modeline-time))

          ;; version control
          "|"
          '(:eval
             (custom-modeline-icon-vc))
          ;; '(:eval (propertize (substring vc-mode 5)
          ;; 			'face 'font-lock-comment-face))

          ;; flycheck status
          '(:eval (when (bound-and-true-p flycheck-mode)
                    "   | "
                    (custom-modeline-flycheck-status)))

          )
        )


      ;; Diplay file size on the modeline
      (size-indication-mode t)

      ;; ** anzu

      ;; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which displays current match and total matches information in the mode-line in various search modes.

      (use-package anzu
        :bind
        (:map isearch-mode-map
          ([remap isearch-query-replace] . anzu-isearch-query-replace)
          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
        :custom-face
        (anzu-mode-line ((nil (:foreground "yellow" :weight bold))))
        :init
        (setq anzu-mode-lighter "")
        (setq anzu-deactivate-region t)
        (setq anzu-search-threshold 1000)
        (setq anzu-replace-threshold 50)
        (setq anzu-replace-to-string-separator " => ")
        :config
        (eval-when-compile
          (global-anzu-mode +1)
          (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
          (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
        )

      ;; Minions

      ;; Group all minor modes in a single menu in the modeline

      (use-package minions
        :config
        (minions-mode 1)
        )

      ;; *********************************
      ;;
      ;; parrot-mode

      (use-package parrot
        ;; :hook
        ;; (parrot-click . parrot-start-animation)
        ;; (after-save . parrot-start-animation)
        ;; (self-insert . parrot-start-animation)
        ;; (parrot-click-hook . flyspell-buffer)
        :config
        ;; To see the party parrot in the modeline, turn on parrot mode:
        (parrot-mode)
        (parrot-set-parrot-type 'default)
        ;; (setq parrot-num-rotations nil) ;; new! make parrot rotate forever
        ;; (setq parrot-animate-parrot t) ;; suposedly animates parrot, same as variable above
        ;;/Rotation function keybindings for evil users
        (define-key evil-normal-state-map (kbd "[r") 'parrot-rotate-prev-word-at-point)
        (define-key evil-normal-state-map (kbd "]r") 'parrot-rotate-next-word-at-point)
        )


      ;; *********************************
      ;;
      ;; nyan-mode

      (use-package nyan-mode
        ;; :if window-system
        :init
        (setq nyan-cat-face-number 4)
        (setq nyan-animate-nyancat t)
        (setq nyan-wavy-trail t)
        :config
        (nyan-mode)
        (nyan-start-animation)
        )
      ) ;; endprogn
    ) ;; endif
  ) ;; enddefineminormode


(provide 'tau-modeline)

;;; tau-modeline.el ends here
