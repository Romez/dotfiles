;;; .emacs --- My config

;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:

(require 'package)

;;; Code:
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'visual) ;; Use relative line numbers

(menu-bar-mode -1)
(tool-bar-mode -1)

;; highlights matching pairs of parentheses
(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

;; fn -> λ ...
(global-prettify-symbols-mode 1)

;; Set the Custom File

(setq custom-file "~/.emacs.d/custom.el")

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(set-frame-font "Source Code Pro-14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)

(add-hook 'c-mode-hook
  (lambda ()
    (setq c-basic-offset 4)
    (setq indent-tabs-mode nil)))

;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package dracula-theme
  :ensure t
  ;; :config (load-theme 'dracula t)
  )

(use-package jbeans-theme
  :ensure t
  ;; :config (load-theme 'jbeans t)
  )

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("C-c )" . paredit-forward-slurp-sexp)
	      ("C-c (" . paredit-forward-barf-sexp)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(setq projectile-globally-ignored-directories
      '(".git" "node_modules" "venv" "build" "class"))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on)
  (setq helm-follow-mode-persistent t))

(use-package helm-ag
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css\\'" . web-mode)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (which-key-add-key-based-replacements "C-c p" "projectile"))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1))

(use-package multiple-cursors
  :ensure t)

(use-package cc-mode
  :ensure t)

(use-package helm
  :ensure t
  :init  (setq helm-command-prefix-key "C-c h")
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-b" . helm-buffers-list)
   ("C-c h o" . helm-occur)
   ("C-c h x" . helm-register)
   ("C-c h g" . helm-do-grep-ag)
   ;; Map C-x r b to register bookmarks in Helm
   ("C-x r b" . helm-filtered-bookmarks)
   ;; To find recent files using Helm, bind previous key map C-x C-r to helm-recentf
   ("C-x C-r" . helm-recentf)
   ;; To resume last Helm session, bind it to the previous key map C-c h h
   ("C-c h h" . helm-resume))
  :config
  ;; Enable helm-mode automatically
  (helm-mode 1)
  ;; Enable helm follow mode globally
  (setq helm-follow-mode-persistent t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode web-mode typescript-ts-mode tsx-ts-mode) . lsp-deferred)
  :commands lsp lsp-deferred
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-clients-clangd-executable "clangd") ; если clangd уже в $PATH
  (lsp-enable-symbol-highlighting t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t))

(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (tsx-mode        . tsx-ts-mode)))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; .emacs ends here
