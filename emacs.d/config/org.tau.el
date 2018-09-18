(require 'org)

(add-hook 'org-mode-hook                                                                      
          (lambda ()                                                                          
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 

;; (setq evil-want-C-i-jump nil)

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

(with-eval-after-load 'ox
  (require 'ox-reveal))

(require 'ox-reveal)

(provide 'org.tau)
;;; org.tau.el ends here...
