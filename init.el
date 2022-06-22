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

(use-package blackout
  :straight t)

(use-package tuareg
  :straight t)

(use-package autorevert
  :straight (:type built-in)
  :blackout (auto-revert-mode))

(use-package rust-mode
  :straight t)

(use-package hideshow
  :straight (:type built-in)
  :blackout (hs-minor-mode))

(use-package elec-pair
  :hook
  (prog-mode . electric-pair-mode))

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

(use-package window
  :straight (:type built-in)
  :config
  (defun +window-size-% (&optional window frame)
    (let ((frame-width (float (frame-width frame)))
          (window-width (float (window-width window))))
      (truncate (* 100 (/ window-width frame-width)))))
  (defun +zoom-window-in/out (&optional window frame)
    (interactive)
    (if (<= (+window-size-% window frame) 50)
        (maximize-window window)
      (progn
        (minimize-window window)
        (other-window 1))))
  :bind
  ("C-c +" . #'+zoom-window-in/out))

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-mode-line '(accented borderless))
  (modus-themes-completions '((matches . (extrabold background intense))
			      (selection . (semibold accented intense))
			      (popup . (accented))))
  (modus-themes-bold-constructs t)
  (modus-themes-operandi-color-overrides '((bg-main . "#fefcf4")))
  :init
  (modus-themes-load-operandi))

(use-package so-long
  :init
  (global-so-long-mode))

(use-package all-the-icons
  :straight t)

(use-package tab-modeline
  :config
  (tab-modeline-mode))

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

(use-package gcmh
  :straight t
  :blackout
  :init
  (gcmh-mode))

(use-package paren
  :straight (:type built-in)
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
  :config
  (define-prefix-command '+consult-prefix-map)
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ("C-c c" . +consult-prefix-map)
  (:map +consult-prefix-map
        ("i" . #'consult-imenu)
        ("I" . #'consult-imenu-multi)
        ("l" . #'consult-line)
        ("M-l" . #'consult-focus-lines)
        ("M-L" . #'consult-keep-lines)
        ("g" . #'consult-ripgrep)
        ("G" . #'consult-git-grep)
        ("f" . #'consult-find)
        ("m" . #'consult-man)))

(use-package eldoc
  :straight (:type built-in)
  :blackout)

(use-package eglot
  :straight t
  :hook
  (eglot-managed-mode . (lambda ()
                          (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose
                                      eldoc-echo-area-use-multiline-p 5)))
  :custom
  (eglot-ignored-server-capabilities (list :documentHighlightProvider))
  (eglot-autoshutdown t))

(use-package flymake
  :straight t
  :custom
  (flymake-no-changes-timeout 2)
  :hook
  (prog-mode . flymake-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-preselect-first nil)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary t)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
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

(use-package cape
  :straight t
  :bind (("C-c p f" . cape-file)
         ("C-c p l" . cape-line)
         ("C-c p \\" . cape-tex)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

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
   '(variable-pitch ((t (:family "Iosevka" :height 140))))
   '(default ((t (:family "Iosevka" :height 140))))))

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
  :bind
  (:map racket-mode-map
        ("C-c \\" . #'racket-insert-lambda)))

(use-package vundo
  :straight t)

(use-package tex
  :straight auctex)

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
 '(bidi-display-reordering 'left-to-right t)
 '(cursor-in-non-selected-windows nil)
 '(highlight-nonselected-windows nil)
 '(inhbit-splash-screen t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((flycheck-clang-language-standard . "c++17")))
 '(tool-bar-mode nil)
 '(indent-tabs-mode nil)
 ;;'(tab-always-indent 'complete)
 '(blink-cursor-mode nil)
 '(truncate-lines t))

(global-set-key (kbd "M-o") #'other-window)

(provide 'init)
;;; init.el ends here.

