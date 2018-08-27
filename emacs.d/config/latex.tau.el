;;; package -- Summary

;;; Commentary:

;;; Code:

; add the beamer class to org so i can export to beamer presentations
(require 'ox-latex)
(add-to-list 'org-latex-classes
	     '("beamer"
	       "\\documentclass\[presentation\]\{beamer\}"
	       ("\\section\{%s\}" . "\\section*\{%s\}")
	       ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
	       ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


(latex-preview-pane-enable)


(provide 'latex.tau)
;;; latex.tau.el ends here
