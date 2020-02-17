;;; corral-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "corral" "corral.el" (0 0 0 0))
;;; Generated autoloads from corral.el

(autoload 'corral-parentheses-backward "corral" "\
Wrap parentheses around sexp, moving point to the closing parentheses.

WRAP-TOGGLE inverts the behavior of closing parenthesis insertion compared to
the `corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-parentheses-forward "corral" "\
Wrap parentheses around sexp, moving point to the closing parentheses.

WRAP-TOGGLE inverts the behavior of opening parenthesis insertion compared to
the `corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-brackets-backward "corral" "\
Wrap brackets around sexp, moving point to the opening bracket.

WRAP-TOGGLE inverts the behavior of closing bracket insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-brackets-forward "corral" "\
Wrap brackets around sexp, moving point to the closing bracket.

WRAP-TOGGLE inverts the behavior of opening bracket insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-braces-backward "corral" "\
Wrap brackets around sexp, moving point to the opening bracket.

WRAP-TOGGLE inverts the behavior of closing bracket insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-braces-forward "corral" "\
Wrap brackets around sexp, moving point to the closing bracket.

WRAP-TOGGLE inverts the behavior of opening bracket insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-single-quotes-backward "corral" "\
Wrap single quotes around sexp, moving point to the opening single quote.

WRAP-TOGGLE inverts the behavior of closing quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-single-quotes-forward "corral" "\
Wrap single quotes around sexp, moving point to the closing single quote.

WRAP-TOGGLE inverts the behavior of opening quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-double-quotes-backward "corral" "\
Wrap double quotes around sexp, moving point to the opening double quote.

WRAP-TOGGLE inverts the behavior of closing quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-double-quotes-forward "corral" "\
Wrap double quotes around sexp, moving point to the closing double quote.

WRAP-TOGGLE inverts the behavior of opening quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-backquote-backward "corral" "\
Wrap double quotes around sexp, moving point to the opening double quote.

WRAP-TOGGLE inverts the behavior of closing quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(autoload 'corral-backquote-forward "corral" "\
Wrap double quotes around sexp, moving point to the closing double quote.

WRAP-TOGGLE inverts the behavior of opening quote insertion compared to the
`corral-default-no-wrap' variable.

\(fn &optional WRAP-TOGGLE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "corral" '("corral-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; corral-autoloads.el ends here
