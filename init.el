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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(use-package recentf
  :init
  (recentf-mode))

(use-package elec-pair
  :disabled
  :init
  (electric-pair-mode))

(use-package paredit
  :straight t
  :hook
  (scheme-mode . paredit-mode)
  (lisp-mode . paredit-mode))

(use-package ibuffer
  :bind
  ("C-x C-b" . (lambda ()
                 (interactive)
                 (ibuffer 'other-window))))

(use-package darwin
  :when (eq system-type 'darwin))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode))

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-completions '((matches . (extrabold background intense))
			      (selection . (semibold accented intense))
			      (popup . (accented))))
  :init
  (modus-themes-load-operandi))

(use-package so-long
  :init
  (global-so-long-mode))

(use-package mood-line
  :straight t
  :init
  (mood-line-mode))

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  :init
  (tab-bar-mode))

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

(use-package gcmh
  :straight t
  :init
  (gcmh-mode))

(use-package paren
  :init
  (show-paren-mode t))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :straight t
  :bind
  ([remap switch-to-buffer] . consult-buffer))

(use-package eglot
  :straight t)

(use-package flymake
  :straight t
  :hook
  (prog-mode . flymake-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-preselect-first nil)
  (corfu-cycle t)
  :hook
  (minibuffer-setup . (lambda ()
                        (when (where-is-internal #'completion-at-point (list (current-local-map)))
                          (corfu-mode))))
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode)))
  :config
  (advice-add #'corfu-insert :after (lambda (&rest _)
                                      (cond
                                       ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
                                        (eshell-send-input))
                                       ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
                                        (comint-send-input)))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-m" . (lambda ()
                   (interactive)
                   (let ((completion-extra-properties corfu--extra)
                         (completion-cycle-threshold nil)
                         (completion-cycling nil))
                     (apply #'consult-completion-in-region completion-in-region--data)))))
  :init
  (global-corfu-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  ("C-;" . embark-act))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package cus-face
  :init
  (custom-set-faces
   '(variable-pitch ((t (:family "Iosevka Aile" :height 160))))
   '(default ((t (:family "Iosevka" :height 160))))))

(use-package minions
  :disabled
  :straight t
  :init
  (minions-mode))

(use-package cc-vars
  :custom
  (c-basic-offset 4))

(use-package pyvenv
  :straight t)

(use-package tree-sitter
  :straight t
  :hook
  (python-mode . tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package sly
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package geiser
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package racket-mode
  :straight t
  :hook
  (racket-mode . racket-xp-mode))

(use-package vundo
  :straight t)

(use-package org
  :straight t
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  ; (org-pretty-entities t)
  (org-ellipsis "…")
  (org-directory "~/Documents/org"))

(use-package org-capture
  :custom
  (org-capture-templates
   `(("j" "Journal Entry"
      entry (file+datetree ,(concat org-directory "/journal.org"))
      "* %?"
      :empty-lines 1)))
  :bind
  ("C-c o c" . org-capture))

(use-package org-agenda
  :bind
  ("C-c o a" . org-agenda)
  :custom
  ( org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package org-modern
  :straight t
  :init
  (global-org-modern-mode))

(use-package tempel
  :straight t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (global-tempel-abbrev-mode))

(use-package python
  :custom
  (python-indent-offset 4))

(use-package magit
  :straight t)

(use-package magit-delta
  :straight t
  :hook
  (magit-mode . magit-delta-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bidi-display-reordering 'left-to-right t)
 '(cursor-in-non-selected-windows nil)
 '(highlight-nonselected-windows nil)
 '(inhbit-splash-screen t)
 '(menu-bar-mode nil)
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((flycheck-clang-language-standard . "c++17")))
 '(tool-bar-mode nil)
 '(indent-tabs-mode nil)
 '(tab-always-indent 'complete)
 '(truncate-lines t))

(global-set-key (kbd "M-o") #'other-window)

(provide 'init)
;;; init.el ends here.

