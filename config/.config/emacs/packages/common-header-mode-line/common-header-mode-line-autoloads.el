;;; common-header-mode-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "common-header-mode-line" "common-header-mode-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from common-header-mode-line.el

(autoload 'common-header-mode-line-add-delayed-update-function "common-header-mode-line" "\


\(fn FUN)" nil nil)

(autoload 'common-header-mode-line-rem-delayed-update-function "common-header-mode-line" "\


\(fn FUN)" nil nil)

(defvar common-mode-line-mode nil "\
Non-nil if Common-Mode-Line mode is enabled.
See the `common-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `common-mode-line-mode'.")

(custom-autoload 'common-mode-line-mode "common-header-mode-line" nil)

(autoload 'common-mode-line-mode "common-header-mode-line" "\
`per-window-mode-line-mode' + `per-frame-mode-line-mode'

\(fn &optional ARG)" t nil)

(defvar common-header-line-mode nil "\
Non-nil if Common-Header-Line mode is enabled.
See the `common-header-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `common-header-line-mode'.")

(custom-autoload 'common-header-line-mode "common-header-mode-line" nil)

(autoload 'common-header-line-mode "common-header-mode-line" "\
`per-window-header-line-mode' + `per-frame-header-line-mode'

\(fn &optional ARG)" t nil)

(defvar common-header-mode-line-mode nil "\
Non-nil if Common-Header-Mode-Line mode is enabled.
See the `common-header-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `common-header-mode-line-mode'.")

(custom-autoload 'common-header-mode-line-mode "common-header-mode-line" nil)

(autoload 'common-header-mode-line-mode "common-header-mode-line" "\
`common-header-line-mode' + `common-mode-line-mode'

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "common-header-mode-line" '("common-")))

;;;***

;;;### (autoloads nil "per-frame-header-mode-line" "per-frame-header-mode-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from per-frame-header-mode-line.el

(defvar per-frame-mode-line-mode nil "\
Non-nil if Per-Frame-Mode-Line mode is enabled.
See the `per-frame-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-frame-mode-line-mode'.")

(custom-autoload 'per-frame-mode-line-mode "per-frame-header-mode-line" nil)

(autoload 'per-frame-mode-line-mode "per-frame-header-mode-line" "\
Toggle the `per-frame-mode-line-mode'.
When active it draws a `mode-line' at the bottom(or top) of
the frame.

\(fn &optional ARG)" t nil)

(defvar per-frame-header-line-mode nil "\
Non-nil if Per-Frame-Header-Line mode is enabled.
See the `per-frame-header-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-frame-header-line-mode'.")

(custom-autoload 'per-frame-header-line-mode "per-frame-header-mode-line" nil)

(autoload 'per-frame-header-line-mode "per-frame-header-mode-line" "\
Toggle the `per-frame-header-line-mode'.
When active it draws a `header-line' at the bottom(or top) of
the frame.

\(fn &optional ARG)" t nil)

(defvar per-frame-header-mode-line-mode nil "\
Non-nil if Per-Frame-Header-Mode-Line mode is enabled.
See the `per-frame-header-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-frame-header-mode-line-mode'.")

(custom-autoload 'per-frame-header-mode-line-mode "per-frame-header-mode-line" nil)

(autoload 'per-frame-header-mode-line-mode "per-frame-header-mode-line" "\
`per-frame-header-line-mode' + `per-frame-mode-line-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "per-frame-header-mode-line" '("with-suspended-per-frame-header-mode-line" "per-frame-")))

;;;***

;;;### (autoloads nil "per-window-header-mode-line" "per-window-header-mode-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from per-window-header-mode-line.el

(defvar per-window-mode-line-mode nil "\
Non-nil if Per-Window-Mode-Line mode is enabled.
See the `per-window-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-window-mode-line-mode'.")

(custom-autoload 'per-window-mode-line-mode "per-window-header-mode-line" nil)

(autoload 'per-window-mode-line-mode "per-window-header-mode-line" "\
Toggle the `per-window-mode-line-mode'. If active
it manages the `mode-line' appearence in visible windows
by changing the buffer-local variable `mode-line-format'
of visible buffers.

\(fn &optional ARG)" t nil)

(defvar per-window-header-line-mode nil "\
Non-nil if Per-Window-Header-Line mode is enabled.
See the `per-window-header-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-window-header-line-mode'.")

(custom-autoload 'per-window-header-line-mode "per-window-header-mode-line" nil)

(autoload 'per-window-header-line-mode "per-window-header-mode-line" "\
Toggle the `per-window-header-line-mode'. If active
it manages the `header-line' appearence in visible windows
by changing the buffer-local variable `header-line-format'
of visible buffers.

\(fn &optional ARG)" t nil)

(defvar per-window-header-mode-line-mode nil "\
Non-nil if Per-Window-Header-Mode-Line mode is enabled.
See the `per-window-header-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `per-window-header-mode-line-mode'.")

(custom-autoload 'per-window-header-mode-line-mode "per-window-header-mode-line" nil)

(autoload 'per-window-header-mode-line-mode "per-window-header-mode-line" "\
`per-window-header-line-mode' + `per-window-mode-line-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "per-window-header-mode-line" '("per-window-")))

;;;***

;;;### (autoloads nil nil ("common-header-mode-line-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; common-header-mode-line-autoloads.el ends here
