;;; org-kanban-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-kanban" "org-kanban.el" (0 0 0 0))
;;; Generated autoloads from org-kanban.el

(autoload 'org-kanban/initialize "org-kanban" "\
Create an org-kanban dynamic block at position ARG.

\(fn &optional ARG)" t nil)

(autoload 'org-kanban/initialize-at-beginning "org-kanban" "\
Create an org-kanban dynamic block at the beginning of the buffer.

\(fn)" t nil)

(autoload 'org-kanban/initialize-at-end "org-kanban" "\
Create an org-kanban dynamic block at the end of the buffer.

\(fn)" t nil)

(autoload 'org-kanban/initialize-here "org-kanban" "\
Create an org-kanban dynamic block at the point.

\(fn)" t nil)

(autoload 'org-dblock-write:kanban "org-kanban" "\
Create the kanban dynamic block.
PARAMS may contain `:mirrored`, `:match`, `:scope`, `:layout` and `:range`.

\(fn PARAMS)" nil nil)

(autoload 'org-kanban/configure-block "org-kanban" "\
Configure the current org-kanban dynamic block.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-kanban" '("org-kanban")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-kanban-autoloads.el ends here
