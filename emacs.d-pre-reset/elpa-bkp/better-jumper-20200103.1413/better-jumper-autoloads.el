;;; better-jumper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "better-jumper" "../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper.el"
;;;;;;  "863a8a4e8d9eb368ec8e8dc23e686691")
;;; Generated autoloads from ../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper.el

(autoload 'better-jumper-set-jump "better-jumper" "\
Set jump point at POS.
POS defaults to point.

\(fn &optional POS)" nil nil)

(autoload 'better-jumper-jump-backward "better-jumper" "\
Jump backward COUNT positions to previous location in jump list.
If COUNT is nil then defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'better-jumper-jump-forward "better-jumper" "\
Jump forward COUNT positions to location in jump list.
If COUNT is nil then defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'better-jumper-get-jumps "better-jumper" "\
Get jumps for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context.

\(fn WINDOW-OR-BUFFER)" nil nil)

(autoload 'better-jumper-set-jumps "better-jumper" "\
Set jumps to JUMPS for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context.

\(fn WINDOW-OR-BUFFER JUMPS)" nil nil)

(autoload 'turn-on-better-jumper-mode "better-jumper" "\
Enable better-jumper-mode in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-better-jumper-mode "better-jumper" "\
Disable `better-jumper-local-mode' in the current buffer.

\(fn)" nil nil)

(autoload 'better-jumper-local-mode "better-jumper" "\
better-jumper minor mode.

\(fn &optional ARG)" t nil)

(defvar better-jumper-mode nil "\
Non-nil if Better-Jumper mode is enabled.
See the `better-jumper-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `better-jumper-mode'.")

(custom-autoload 'better-jumper-mode "better-jumper" nil)

(autoload 'better-jumper-mode "better-jumper" "\
Toggle Better-Jumper-Local mode in all buffers.
With prefix ARG, enable Better-Jumper mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Better-Jumper-Local mode is enabled in all buffers where
`turn-on-better-jumper-mode' would do it.
See `better-jumper-local-mode' for more information on Better-Jumper-Local mode.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "better-jumper"
;;;;;;  "../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "better-jumper" '("better-jumper-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/better-jumper-20200103.1413/better-jumper.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; better-jumper-autoloads.el ends here
