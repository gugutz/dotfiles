;;; eldoc-box-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eldoc-box" "eldoc-box.el" (0 0 0 0))
;;; Generated autoloads from eldoc-box.el

(autoload 'eldoc-box-hover-mode "eldoc-box" "\
Displays hover documentations in a childframe.
The default position of childframe is upper corner.

\(fn &optional ARG)" t nil)

(autoload 'eldoc-box-hover-at-point-mode "eldoc-box" "\
A convenient minor mode to display doc at point.
You can use C-g to hide the doc.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eldoc-box" '("eldoc-box-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-box-autoloads.el ends here
