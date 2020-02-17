;;; js-import-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-import" "js-import.el" (0 0 0 0))
;;; Generated autoloads from js-import.el

(autoload 'js-import "js-import" "\
Import default export from modules on your current project or dependencies.

With one prefix argument, import exported symbols.
With two prefix arguments, import all exports as a symbol.

\(fn ARG)" t nil)

(autoload 'js-import-dev "js-import" "\
Import default export from modules on your current project or dependencies.

With one prefix argument, import exported symbols.
With two prefix arguments, import all exports as a symbol.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-import" '("js-import-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-import-autoloads.el ends here
