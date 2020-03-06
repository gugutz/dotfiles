

;; New emacs 27 'package-quickstart' feature
;; package.el precomputes a big autoloads file so that activation of packages can be done much faster. It also causes variables like package-user-dir and package-load-list to be consulted when 'package-quickstart-refresh' is run rather than at startup so you don't need to set them in your early init file.
(setq package-quickstart t)

;; We're going to set the =load-path= ourselves and avoid calling =(package-initilize)= (forperformance reasons) so we need to set =package--init-file-ensured= to true to tell =package.el= to not automatically call it on our behalf.
;; Additionally we're setting =package-enable-at-startup= to nil so that packages will not automatically be loaded for us since =use-package= will be handling that.

;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
;; (eval-and-compile
;;   (setq load-prefer-newer t
;;     package-user-dir (concat user-emacs-directory "elpa/")
;;     package-enable-at-startup nil
;;     package--init-file-ensured t)

;;   (unless (file-directory-p package-user-dir)
;;     (make-directory package-user-dir t)))


(unless (assq 'menu-bar-lines default-frame-alist)
  ;; We do this in early-init.el too, but in case the user is on Emacs 26 we do
  ;; it here too: disable tool and scrollbars, as Doom encourages
  ;; keyboard-centric workflows, so these are just clutter (the scrollbar also
  ;; impacts performance).
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))
