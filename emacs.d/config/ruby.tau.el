(require 'ruby-mode)

(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(setq enh-ruby-program "~/.rbenv/shims/ruby") ; so that still works if ruby points to ruby1.8

(setq-default
  ruby-use-encoding-map nil
  ruby-insert-encoding-magic-comment nil)

(after 'enh-ruby-mode
            ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
            ;; prog-mode: we run the latter's hooks anyway in that case.
            (add-hook 'ruby-mode-hook
                      (lambda ()
                        (unless (derived-mode-p 'prog-mode)
                          (run-hooks 'prog-mode-hook)))))

(add-hook 'enh-ruby-mode-hook 'subword-mode)

(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

(after 'page-break-lines
            (push 'ruby-mode page-break-lines-modes))

(require 'rspec-mode)

(require 'inf-ruby)

(require 'ruby-compilation)

(after 'enh-ruby-mode
            (let ((m ruby-mode-map))
              (define-key m [S-f7] 'ruby-compilation-this-buffer)
              (define-key m [f7] 'ruby-compilation-this-test)))

(after 'ruby-compilation
            (defalias 'rake 'ruby-compilation-rake))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(eval-after 'company
  '(push 'company-robe company-backends))

(add-hook 'robe-mode-hook 'ac-robe-setup)

;; (require 'yari)
;; (defalias 'ri 'yari)

(require 'goto-gem)

(require 'bundler)

(when (maybe-require 'yard-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (after 'yard-mode
              (diminish 'yard-mode)))

;;; ERB
(require 'mmm-mode)
(defun sanityinc/ensure-mmm-erb-loaded ()
  (require 'mmm-erb))

(require 'derived)

(defun sanityinc/set-up-mode-for-erb (mode)
  (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
  (dolist (mode html-erb-modes)
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

(mapc 'sanityinc/set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
(sanityinc/set-up-mode-for-erb 'yaml-mode)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))

;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------

;; Needs to run after rinari to avoid clobbering font-lock-keywords?

;; (require 'mmm-mode)
;; (eval-after 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;(add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)

(provide 'ruby.tau)
;;; ruby.tau ends here

(require 'rubocop)
