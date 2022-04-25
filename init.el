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

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

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

(use-package lsp-mode
  :straight t)

(use-package consult-lsp
  :straight t)

(use-package flycheck
  :straight t
  :hook
  (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :straight t)

(use-package corfu
  :straight t
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
   '(variable-pitch ((t (:family "Iosevka Aile" :height 120))))
   '(default ((t (:family "Iosevka" :height 120))))))

(use-package minions
  :straight t
  :init
  (minions-mode))

(use-package cc-vars
  :custom
  (c-basic-offset 4))

(use-package tree-sitter
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package sly
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package vundo
  :straight t)

(use-package ibuffer
  :bind
  ("C-x C-b" . (lambda ()
		 (interactive)
		 (ibuffer 'other-window))))

(use-package org
  :straight t
  :custom
 (org-auto-align-tags nil)
 (org-tags-column 0)
 (org-catch-invisible-edits 'show-and-error)
 (org-special-ctrl-a/e t)
 (org-insert-heading-respect-content t)
 (org-hide-emphasis-markers t)
 (org-pretty-entities t)
 (org-ellipsis "…"))

(use-package org-agenda
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

(custom-set-variables
 '(truncate-lines t)
 '(ring-bell-function 'ignore)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
