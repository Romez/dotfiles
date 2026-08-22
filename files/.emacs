;;; .emacs --- My config

;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:

(require 'package)

;;; Code:
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(menu-bar-mode -1)
(tool-bar-mode -1)

;; highlights matching pairs of parentheses
(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

;; fn -> λ
(global-prettify-symbols-mode 1)

(global-whitespace-mode 1)

(setq-default whitespace-style '(face spaces empty tabs trailing space-mark tab-mark ))

;; Set the Custom File

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(set-frame-font "Source Code Pro-14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true" nil t)

(setq c-basic-offset 4)
(setq indent-tabs-mode nil)

;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

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

;; (use-package flycheck
;;   :ensure t
;;   ;; :config (global-flycheck-mode)
;;   )

;; (use-package flycheck-clj-kondo
;;   :ensure t)

;; (use-package yaml-mode
;;   :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1))

(use-package kotlin-mode
  :ensure t
  :mode "\\.kts?\\'")

;; (use-package multiple-cursors
;;   :ensure t)

;; (use-package go-mode
;;   :ensure t)

(use-package helm
  :ensure t
  :demand t
  :init  (setq helm-command-prefix-key "C-c h")

  :custom
  ;; Enable helm follow mode globally
  (setq helm-follow-mode-persistent t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)

  :bind
  (("M-x" . helm-M-x)
   ;; ("C-x C-f" . helm-find-files)
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
  )

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config (helm-projectile-on))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((web-mode
          typescript-ts-mode
          tsx-ts-mode)
         . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-idle-delay 0.3)
  (lsp-enable-snippet t)
  )

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package company
  :ensure t
  :config (global-company-mode 1))

;; (use-package move-text
;;   :ensure t
;;   :init (move-text-default-bindings))

(with-eval-after-load 'whitespace
  (set-face-attribute 'whitespace-space nil
                      :foreground "gray25"
                      :background nil
                      :weight 'light)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "gray25"
                      :background nil
                      :weight 'light))

;;; .emacs ends here
