;;; package -- Summary
;############################################
;; ORG settings

;############################################
;;; Commentary:

;; these are the bindings i eventually got to
;; while trying to resolve conflicts
;; with evil disabling native Emacs bindings

;############################################
;;; Code:

; require Evil related packages

(require 'org)


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

;; (defun org-mode-export-hook ()
  "This exports to diffenent outputs everytime the file is saved.
This will be added to org-mode-hook, so it only activates on ORG files.
Generates outputs in these formats:
- PDF
- HTML
- RevealJS."
   ;; (add-hook 'after-save-hook 'org-beamer-export-to-pdf t t)
   ;; (add-hook 'after-save-hook 'org-reveal-export-to-html t t))
; Finally adds the above hook in org-mode-hook.
;; (add-hook 'org-mode-hook #'org-mode-export-hook)


;###############################################################################
"As pandoc supports many number of formats, initial org-export-dispatch
shortcut menu does not show full of its supported formats. You can customize
org-pandoc-menu-entry variable (and probably restart Emacs) to change its
default menu entries.
If you want delayed loading of `ox-pandoc’ when org-pandoc-menu-entry
is customized, please consider the following settings in your init file"
(with-eval-after-load 'ox
  (require 'ox-pandoc))

(require 'ox-pandoc)

;; default options for all output formats
(setq org-pandoc-options '((standalone . t)))
;; cancel above settings only for 'docx' format
(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;###############################################################################
;;; ReveaJS org-reveal:
; This makes the options to export to RevealJS appear on the exporter menu (C-c C-e)
(with-eval-after-load 'ox
  (require 'ox-reveal))

(require 'ox-reveal)

(provide 'org.tau)
;;; org.tau.el ends here

