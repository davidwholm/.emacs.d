(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-completions '((matches . (extrabold background intense))
			      (selection . (semibold accented intense))
			      (popup . (accented))))
  :init
  (modus-themes-load-operandi))

(use-package paren
  :init
  (show-paren-mode t))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package consult
  :straight t
  :bind
  ([remap switch-to-buffer] . consult-buffer))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package faces
  :config
  (set-face-attribute 'default nil :font "Iosevka"))

(use-package cc-vars
  :custom
  (c-basic-offset 4))

(use-package sly
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))

(custom-set-variables
 '(truncate-lines t)
 '(ring-bell-function 'ignore)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
