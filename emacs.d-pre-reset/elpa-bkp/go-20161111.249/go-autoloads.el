;;; go-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go" "go.el" (0 0 0 0))
;;; Generated autoloads from go.el

(autoload 'go-play "go" "\
Play a game of GO.

\(fn)" t nil)

(autoload 'go-view-sgf "go" "\
View an SGF file.

\(fn &optional FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go" '("go-instantiate")))

;;;***

;;;### (autoloads nil "go-api" "go-api.el" (0 0 0 0))
;;; Generated autoloads from go-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-api" '("go-" "defgeneric-w-setf" "ignoring-unsupported")))

;;;***

;;;### (autoloads nil "go-board" "go-board.el" (0 0 0 0))
;;; Generated autoloads from go-board.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-board" '("go-" "set-go-" "white-piece" "with-" "black-piece" "board" "update-display" "point-of-pos" "pieces-to-board" "player-to-string" "remove-dead" "apply-" "alive-p" "neighbors" "clear-labels" "other-color" "make-board" "move-type" "*history*" "*size*" "*white*" "*b" "*t" "*autoplay*" "*go-board-overlays*")))

;;;***

;;;### (autoloads nil "go-board-faces" "go-board-faces.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from go-board-faces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-board-faces" '("go-board-")))

;;;***

;;;### (autoloads nil "go-util" "go-util.el" (0 0 0 0))
;;; Generated autoloads from go-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-util" '("go-" "make-go-insertion-filter" "*go-partial-line*" "set-aget" "sym-cat" "num-to-char" "curry" "compose" "char-to-num" "until" "un-ear-muffs" "ear-muffs" "take" "transpose-array" "pos-to-index" "aget" "alistp" "rpush" "range" "indexed")))

;;;***

;;;### (autoloads nil "list-buffer" "list-buffer.el" (0 0 0 0))
;;; Generated autoloads from list-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "list-buffer" '("list" "*buffer-" "*enter-function*" "*refresh-function*")))

;;;***

;;;### (autoloads nil nil ("go-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-autoloads.el ends here
