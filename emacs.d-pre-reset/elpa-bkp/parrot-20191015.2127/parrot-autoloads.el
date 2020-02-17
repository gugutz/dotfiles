;;; parrot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "parrot" "parrot.el" (0 0 0 0))
;;; Generated autoloads from parrot.el

(defvar parrot-mode nil "\
Non-nil if Parrot mode is enabled.
See the `parrot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `parrot-mode'.")

(custom-autoload 'parrot-mode "parrot" nil)

(autoload 'parrot-mode "parrot" "\
Use Parrot to show when you're rotating.
You can customize this minor mode, see option `parrot-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parrot" '("parrot-")))

;;;***

;;;### (autoloads nil "parrot-rotate" "parrot-rotate.el" (0 0 0 0))
;;; Generated autoloads from parrot-rotate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parrot-rotate" '("pulse-flag" "parrot-rotate-")))

;;;***

;;;### (autoloads nil nil ("parrot-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; parrot-autoloads.el ends here
