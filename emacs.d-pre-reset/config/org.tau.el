(require 'org)

(add-hook 'org-mode-hook
          (lambda ()
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))

;; (setq evil-want-C-i-jump nil)

(setq org-log-done 'time)

(setq org-log-done 'note)

(defun toggle-org-html-export-on-save ()
  "Make Emacs auto-export to HTML when org file is saved.
Enable calling this function from the file with <M-x>."
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun org-mode-export-hook ()
  "This exports to diffenent outputs everytime the file is saved.
This will be added to org-mode-hook, so it only activates on ORG files.
Generates outputs in these formats:
- PDF
- HTML
- RevealJS."
   (add-hook 'after-save-hook 'org-beamer-export-to-pdf t t)
   (add-hook 'after-save-hook 'org-reveal-export-to-html t t))

; Finally adds the above hook in org-mode-hook.
;; (add-hook 'org-mode-hook #'org-mode-export-hook)

(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(after 'org
  (require 'evil-org)
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
;; (evil-org-agenda-set-keys)

(with-eval-after-load 'ox
  (require 'ox-pandoc))

(require 'ox-pandoc)

;; default options for all output formats
(setq org-pandoc-options '((standalone . t)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "luatex")))
;; special extensions for markdown_github output
(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

(setq org-enable-bootstrap-support t)

(use-package ox-reveal
  :after (ox)
  :config (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
)

(provide 'org.tau)
;;; elixir.tau.el ends here...
