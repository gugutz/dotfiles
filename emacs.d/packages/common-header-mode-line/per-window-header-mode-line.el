;;; per-window-header-mode-line.el --- per-window header-mode-line.

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Date: 02/04/2017 16:48
;; License: GPL either version 3 or any later version
;; URL: http://github.com/Bad-ptr/common-header-mode-line.el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file is autogenerated. Do not edit manually.
;; This file is the part of the common-header-mode-line package

;; Draws per-window header-mode-line.

;; To use it, first install common-header-mode-line package, then:

;; (require 'per-window-header-mode-line)
;; (per-window-header-line-mode)
;; (per-window-mode-line-mode)

;; M-x customize-group RET per-window-header-mode-line RET
;; M-x customize-group RET per-window-header-line RET
;; M-x customize-group RET per-window-mode-line RET

;;; Code:

(defvar per-window-mode-line-mode nil)

(defvar per-window-header-line-mode nil)

(defvar per-window-header-mode-line-mode nil)

(defgroup per-window-header-mode-line nil "Customize per-window-header-mode-line." :prefix "per-window-header-mode-line-" :group 'common-header-mode-line)

(defcustom per-window-header-mode-line-ignore-buffer-functions nil "Ignore buffer(argument) if one of these functions return non nil." :group 'per-window-header-mode-line :type 'hook)

(defgroup per-window-mode-line nil "Customize per-window-mode-lie." :prefix "per-window-mode-line-" :group 'per-window-header-mode-line)

(defgroup per-window-header-line nil "Customize per-window-header-lie." :prefix "per-window-header-line-" :group 'per-window-header-mode-line)

(defcustom per-window-mode-line-format-function #'per-window-mode-line--format-function "Function, takes window, returns mode-line-format for window." :group 'per-window-mode-line :type 'function :set
  #'(lambda
      (sym val)
      (custom-set-default sym val)
      (when per-window-mode-line-mode
	(per-window-header-mode-line-update-all-windows t))))

(defcustom per-window-header-line-format-function #'per-window-header-line--format-function "Function, takes window, returns header-line-format for window." :group 'per-window-header-line :type 'function :set
  #'(lambda
      (sym val)
      (custom-set-default sym val)
      (when per-window-header-line-mode
	(per-window-header-mode-line-update-all-windows t))))

(defvar-local per-window-mode-line--saved-emacs-format nil "Default format.")

(defvar-local per-window-header-line--saved-emacs-format nil "Default format.")

(defvar-local per-window-mode-line--face-remap-cookies nil "Per-buffer face-remap cookie.")

(defvar-local per-window-header-line--face-remap-cookies nil "Per-buffer face-remap cookie.")

(defface per-window-mode-line-active-face
  '((default :inherit mode-line :height 5))
  "Face to use for per-window mode-line when window is active." :group 'per-window-mode-line)

(defface per-window-header-line-active-face
  '((default :inherit header-line :height 5))
  "Face to use for per-window header-line when window is active." :group 'per-window-header-line)

(defface per-window-mode-line-inactive-face
  '((default :inherit mode-line-inactive :height 3))
  "Face to use for per-window mode-line when window is inactive." :group 'per-window-mode-line)

(defface per-window-header-line-inactive-face
  '((default :inherit mode-line-inactive :height 3))
  "Face to use for per-window mode-line when window is inactive." :group 'per-window-header-line)

(defun per-window-mode-line--format-function
    (win)
  "Default format function." " ")

(defun per-window-header-line--format-function
    (win)
  "Default format function." nil)

(defun per-window-mode-line--format
    (win)
  (funcall per-window-mode-line-format-function win))

(defun per-window-header-line--format
    (win)
  (funcall per-window-header-line-format-function win))

(defun per-window-mode-line--update-window
    (win &optional buf)
  (when per-window-mode-line-mode
    (unless
	(buffer-live-p buf)
      (setq buf
	    (window-buffer win)))
    (with-current-buffer buf
      (unless per-window-mode-line--saved-emacs-format
	(setq-local per-window-mode-line--saved-emacs-format
		    (or
		     (buffer-local-value 'mode-line-format buf)
		     :nil)))
      (progn
	(unless per-window-mode-line--face-remap-cookies
	  (push
	   (face-remap-add-relative 'mode-line-inactive 'per-window-mode-line-inactive-face)
	   per-window-mode-line--face-remap-cookies)
	  (push
	   (face-remap-add-relative 'mode-line 'per-window-mode-line-active-face)
	   per-window-mode-line--face-remap-cookies)))
      (setq-local mode-line-format
		  (per-window-mode-line--format win)))))

(defun per-window-header-line--update-window
    (win &optional buf)
  (when per-window-header-line-mode
    (unless
	(buffer-live-p buf)
      (setq buf
	    (window-buffer win)))
    (with-current-buffer buf
      (unless per-window-header-line--saved-emacs-format
	(setq-local per-window-header-line--saved-emacs-format
		    (or
		     (buffer-local-value 'header-line-format buf)
		     :nil)))
      (progn
	(when per-window-header-line--face-remap-cookies
	  (face-remap-remove-relative per-window-header-line--face-remap-cookies)
	  (setq-local per-window-header-line--face-remap-cookies nil))
	(setq-local per-window-header-line--face-remap-cookies
		    (face-remap-add-relative 'header-line
					     (if
						 (eq win
						     (selected-window))
						 'per-window-header-line-active-face 'per-window-header-line-inactive-face))))
      (setq-local header-line-format
		  (per-window-header-line--format win)))))

(defun per-window-header-mode-line--update-window
    (win)
  (let
      ((buf
	(window-buffer win)))
    (unless
	(run-hook-with-args-until-success 'per-window-header-mode-line-ignore-buffer-functions buf)
      (progn
	(progn
	  (per-window-mode-line--update-window win buf)
	  (per-window-header-line--update-window win buf))))))

(defun per-window-header-mode-line-update-all-windows
    (&optional all-frames)
  (interactive "P")
  (setq all-frames
	(not
	 (null all-frames)))
  (save-excursion
    (let*
	((start-win
	  (if
	      (minibuffer-window-active-p
	       (selected-window))
	      (minibuffer-selected-window)
	    (selected-window)))
	 (cwin
	  (next-window start-win 0 all-frames)))
      (while
	  (not
	   (eq cwin start-win))
	(per-window-header-mode-line--update-window cwin)
	(setq cwin
	      (next-window cwin 0 all-frames)))
      (per-window-header-mode-line--update-window start-win))))

(defun per-window-header-mode-line-generic-hook
    (&rest args)
  (when
      (or per-window-header-line-mode per-window-mode-line-mode)
    (per-window-header-mode-line-update-all-windows))
  t)

(defun per-window-mode-line--activate nil
  (common-header-mode-line-add-delayed-update-function #'per-window-header-mode-line-generic-hook)
  (per-window-header-mode-line-update-all-windows t))

(defun per-window-header-line--activate nil
  (common-header-mode-line-add-delayed-update-function #'per-window-header-mode-line-generic-hook)
  (per-window-header-mode-line-update-all-windows t))

(defun per-window-mode-line--deactivate nil
  (unless
      (or per-window-header-line-mode per-window-mode-line-mode)
    (common-header-mode-line-rem-delayed-update-function #'per-window-header-mode-line-generic-hook))
  (dolist
      (buf
       (buffer-list))
    (with-current-buffer buf
      (when per-window-mode-line--face-remap-cookies
	(while per-window-mode-line--face-remap-cookies
	  (face-remap-remove-relative
	   (pop per-window-mode-line--face-remap-cookies))))
      (when per-window-mode-line--saved-emacs-format
	(if
	    (eq :nil per-window-mode-line--saved-emacs-format)
	    (setq-local mode-line-format nil)
	  (setq-local mode-line-format per-window-mode-line--saved-emacs-format)))
      (setq-local per-window-mode-line--saved-emacs-format nil)))
  (progn
    (progn
      (progn
	(face-spec-recalc 'mode-line-inactive nil)
	(face-spec-recalc 'mode-line nil))
      (progn nil
	     (face-spec-recalc 'header-line nil)))))

(defun per-window-header-line--deactivate nil
  (unless
      (or per-window-header-line-mode per-window-mode-line-mode)
    (common-header-mode-line-rem-delayed-update-function #'per-window-header-mode-line-generic-hook))
  (dolist
      (buf
       (buffer-list))
    (with-current-buffer buf
      (when per-window-header-line--face-remap-cookies
	(progn
	  (face-remap-remove-relative per-window-header-line--face-remap-cookies)
	  (setq-local per-window-header-line--face-remap-cookies nil)))
      (when per-window-header-line--saved-emacs-format
	(if
	    (eq :nil per-window-header-line--saved-emacs-format)
	    (setq-local header-line-format nil)
	  (setq-local header-line-format per-window-header-line--saved-emacs-format)))
      (setq-local per-window-header-line--saved-emacs-format nil)))
  (progn
    (progn
      (progn
	(face-spec-recalc 'mode-line-inactive nil)
	(face-spec-recalc 'mode-line nil))
      (progn nil
	     (face-spec-recalc 'header-line nil)))))

;;;###autoload
(define-minor-mode per-window-mode-line-mode "Toggle the `per-window-mode-line-mode'. If active\nit manages the `mode-line' appearence in visible windows\nby changing the buffer-local variable `mode-line-format'\nof visible buffers." :require 'per-window-header-mode-line :group 'per-window-mode-line :init-value nil :global t
  (if per-window-mode-line-mode
      (per-window-mode-line--activate)
    (per-window-mode-line--deactivate)))

;;;###autoload
(define-minor-mode per-window-header-line-mode "Toggle the `per-window-header-line-mode'. If active\nit manages the `header-line' appearence in visible windows\nby changing the buffer-local variable `header-line-format'\nof visible buffers." :require 'per-window-header-mode-line :group 'per-window-header-line :init-value nil :global t
  (if per-window-header-line-mode
      (per-window-header-line--activate)
    (per-window-header-line--deactivate)))

;;;###autoload
(define-minor-mode per-window-header-mode-line-mode "`per-window-header-line-mode' + `per-window-mode-line-mode'." :require 'per-window-header-mode-line :group 'per-window-header-mode-line :init-value nil :global t
  (if per-window-header-mode-line-mode
      (progn
	(progn
	  (per-window-mode-line-mode 1)
	  (per-window-header-line-mode 1)))
    (progn
      (progn
	(per-window-mode-line-mode -1)
	(per-window-header-line-mode -1)))))

(provide 'per-window-header-mode-line)


;;; per-window-header-mode-line.el ends here