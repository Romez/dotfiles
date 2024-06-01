;;; .emacs --- My config

;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:

(require 'package)

;;; Code:

(menu-bar-mode -1)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer 'always)
  (setq cider-auto-select-error-buffer t))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

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
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t))

(global-set-key (kbd "C-c h o") 'helm-occur)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(setq projectile-globally-ignored-directories
      '(".git" "node_modules" "venv" "build"))

(setq projectile-globally-ignored-directories
      '(".git" "node_modules" "venv" "build" "class"))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t)

(use-package company
  :ensure t
  :hook ((after-init . global-company-mode)))

(use-package racket-mode
  :ensure t
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(use-package yaml-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  :hook
  (clojure-mode . lsp))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Prefix for LSP commands
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet t)
  :hook ((web-mode . lsp-deferred)
	 (js-mode . lsp)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Use company-lsp as a backend
(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1))

(use-package elm-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)))

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; highlights matching pairs of parentheses
(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

;; fn -> λ ...
(global-prettify-symbols-mode 1)

;; To enable whitespace-mode globally
(global-whitespace-mode 1)

;; Set whitespace-mode to highlight lines longer than 120 characters
(setq whitespace-line-column 120)

;; Use GNU ls from coreutils
(setq insert-directory-program "gls")

;; Set the Custom File
(setq custom-file "~/.emacs.d/custom.el")

;;; .emacs ends here
