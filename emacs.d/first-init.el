;;; package -- Summary
;############################################
;; evil

;############################################
;;; Commentary:

; this files serves only to compile the parts
; marked with tangle in the main ORG file (emacs.org)

;############################################
;;; Code:


;; We can't tangle without org!
(require 'org)
;; Open the configuration
(find-file (concat user-emacs-directory "emacs.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "init.el"))
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))
