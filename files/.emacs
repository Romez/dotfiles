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

;;; Packages

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package dracula-theme
  :ensure t
  ; :config (load-theme 'dracula t)
  )

(use-package vs-light-theme
  :ensure t
  ; :config (load-theme 'vs-light t)
  )

(use-package jbeans-theme
  :ensure t
  :config (load-theme 'jbeans t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("C-c )" . paredit-forward-slurp-sexp)
	      ("C-c (" . paredit-forward-barf-sexp)))

(use-package helm
  :ensure t
  :demand t
  :init
  ;; Set helm prefix key
  (setq helm-command-prefix-key "C-c h")
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

(use-package racket-mode
  :ensure t
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(use-package yaml-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  :hook
  (clojure-mode . lsp))

(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((clojure-mode . lsp-deferred)
	 (web-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Set prefix for lsp-command-keymap
  :config
  (setq lsp-prefer-flymake nil ; Use lsp-ui and flycheck instead of flymake
	lsp-enable-snippet t
	lsp-enable-indentation nil); CIDER now supports safe indentation
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (which-key-add-key-based-replacements
   "C-c l" "lsp"   ;; Register lsp prefix
   "C-c p" "projectile"))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1))

(use-package elm-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t)

;; enable upcase-region
(put 'upcase-region 'disabled nil)

;;; .emacs ends here
