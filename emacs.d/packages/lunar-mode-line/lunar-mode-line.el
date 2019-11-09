;;; lunar-mode-line.el --- display lunar phase in mode line of Emacs -*-coding: utf-8 -*-

;; Copyright (C) 2006-2014 Ben Voui

;; Author: Ben Voui <intrigeri@boum.org>
;; Maintainer: intrigeri@boum.org
;; Contributor: atykhonov@gmail.com
;; Keywords: extensions
;; Status: Works in Emacs 22 (in case of lunar-mode-line-use-images is nil).
;; For graphic representation it is probably required >=Emacs 24.3
;; Created: 2006-05-22
;; Updated: 2014-04-09

;; URL: https://github.com/atykhonov/lunar-mode-line

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, you can either send email to this program's
;; maintainer or write to: The Free Software Foundation, Inc.; 59 Temple
;; Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by battery.el, by Ralph Schleicher <rs@nunatak.allgaeu.org>.
;; Graphic representation inspired by nyan-mode, by Jacek "TeMPOraL" Zlydach
;; <temporal.pl@gmail.com>

;;; Code:

(require 'calendar)
(require 'lunar)
(require 'timer)

(defvar lunar-mode-line-string nil
  "String to display in the mode line.")

(put 'lunar-mode-line-string 'risky-local-variable t)

(defconst lunar-mode-line-directory (file-name-directory (or load-file-name buffer-file-name)))

(defconst lunar-mode-line-image-new (concat lunar-mode-line-directory "images/new.png"))
(defconst lunar-mode-line-image-full (concat lunar-mode-line-directory "images/full.png"))
(defconst lunar-mode-line-image-first-quarter (concat lunar-mode-line-directory "images/first_quarter.png"))
(defconst lunar-mode-line-image-last-quarter (concat lunar-mode-line-directory "images/last_quarter.png"))

(defgroup lunar-mode-line nil
  "Display lunar phase in mode line of Emacs."
  :group 'modeline)

(defcustom lunar-mode-line-update-interval 3600
  "*Seconds between updates of lunar phase in the mode line."
  :type 'integer
  :group 'lunar-mode-line)

(defcustom lunar-mode-line-prefix ""
  "Text to display before the lunar phase icon in the mode-line."
  :type 'string
  :group 'lunar-mode-line)

(defcustom lunar-mode-line-suffix ""
  "Text to display after the lunar phase icon in the mode-line."
  :type 'string
  :group 'lunar-mode-line)

(defcustom lunar-mode-line-use-images t
  "Indicates whether to display lunar phase icon as an image."
  :type 'string
  :group 'lunar-mode-line)

(defvar lunar-mode-line-timer nil
  "Interval timer object.")

(defvar lunar-mode-line-text-representation-alist
  '((0 . "(/)")
    (1 . "|)")
    (2 . "(·)")
    (3 . "(|"))
  "Alist mapping phase numbers to the strings used to represent them in the mode-line.
  Note: 0: New Moon, 1: First Quarter Moon, 2: Full Moon, 3: Last
  Quarter Moon")

(defvar lunar-mode-line-image-alist
  '((0 . lunar-mode-line-image-new)
    (1 . lunar-mode-line-image-first-quarter)
    (2 . lunar-mode-line-image-full)
    (3 . lunar-mode-line-image-last-quarter))
  "Alist mapping phase numbers to the images used to represent them in the mode-line.
  Note: 0: New Moon, 1: First Quarter Moon, 2: Full Moon, 3: Last
  Quarter Moon")

;;;###autoload
(defun display-lunar-phase ()
  "Display lunar phase information in the echo area."
  (interactive)
  (message "%s : %s"
	   (lunar-mode-line-current-phase-name)
	   (lunar-mode-line-current-phase-text-representation)))

;;;###autoload
(define-minor-mode display-lunar-phase-mode
  "Toggle display of lunar phase information in the mode line.
With a numeric arg, enable this display if arg is positive.

The mode line will be updated automatically every
`lunar-mode-line-update-interval' seconds."
  :global t :group 'lunar-mode-line
  (setq lunar-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and lunar-mode-line-timer (cancel-timer lunar-mode-line-timer))
  (if (not display-lunar-phase-mode)
      (setq global-mode-string
	    (delq 'lunar-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'lunar-mode-line-string t)
    (setq lunar-mode-line-timer (run-at-time nil lunar-mode-line-update-interval
					     'lunar-mode-line-update-handler))
    (lunar-mode-line-update)))

(defun lunar-mode-line-update-handler ()
  (lunar-mode-line-update)
  (sit-for 0))

(defun lunar-mode-line-create ()
  (propertize "-" 'display
              (create-image (lunar-mode-line-current-phase-image-filename)
                            'png nil
                            :ascent 'center
                            :background (face-background 'mode-line))
              'help-echo "Lunar phase information"))

(defun lunar-mode-line-update ()
  "Update lunar phase information in the mode line."  
  (if lunar-mode-line-use-images
      (setq lunar-mode-line-string '(:eval (lunar-mode-line-create)))
    (setq lunar-mode-line-string
          (propertize 
           (concat
            lunar-mode-line-prefix
            (lunar-mode-line-current-phase-text-representation)
            lunar-mode-line-suffix)
           'help-echo "Lunar phase information")))
  (force-mode-line-update))

;; Aux. functions

(defun lunar-mode-line-current-phase ()
  (let* ((date (list (calendar-current-date)))
	 (day (extract-calendar-day (car date)))
	 (month (extract-calendar-month (car date)))
	 (year (extract-calendar-year (car date)))
	 (phase-list (lunar-phase-list month year))
	 (cur-phase (car phase-list))
	 (next-phase (car phase-list)))
    (while (calendar-date-compare next-phase date)
      (setq cur-phase (car phase-list))
      (setq phase-list (cdr phase-list))
      (setq next-phase (car phase-list)))
    (car (cdr (cdr cur-phase)))))

(defun lunar-mode-line-current-phase-name ()
  (lunar-phase-name (lunar-mode-line-current-phase)))

(defun lunar-mode-line-current-phase-text-representation ()
  (cdr (assoc (lunar-mode-line-current-phase) lunar-mode-line-text-representation-alist)))

(defun lunar-mode-line-current-phase-image-filename ()
  (symbol-value (cdr (assoc (lunar-mode-line-current-phase) lunar-mode-line-image-alist))))

(provide 'lunar-mode-line)

;;; lunar-mode-line.el ends here
