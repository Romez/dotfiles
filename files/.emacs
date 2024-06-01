;;; .emacs --- My config

;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Commentary:

(require 'package)

;;; Code:

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
  (projectile-mode +1)
  (projectile-register-project-type 'racket '("info.rkt")
				    :project-file "info.rkt"
				    :test "raco test ."
				    :install "raco pkg install"
				    :package "raco pkg create --source $(pwd)"
				    :test-suffix "_test"))

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

(use-package paredit
  :ensure t
  :hook ((cider-repl-mode . paredit-mode)
         (cider-mode . paredit-mode)
         (clojure-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("C-c s" . paredit-forward-slurp-sexp)
	      ("C-c b" . paredit-forward-barf-sexp))
  :config (dolist (m '(emacs-lisp-mode-hook
		       racket-mode-hook
		       racket-repl-mode-hook))
	    (add-hook m #'paredit-mode)))

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
  (global-flycheck-mode)
    ;; Overwrite existing scss-stylelint checker to not use --syntax
  (flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
;; "--syntax" "scss"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :modes (scss-mode)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode +1))

(use-package elm-mode
  :ensure t)

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

(global-prettify-symbols-mode 1)

;; To enable whitespace-mode globally
(global-whitespace-mode 1)

;; Set whitespace-mode to highlight lines longer than 120 characters
(setq whitespace-line-column 120)

(setq insert-directory-program "gls")

(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f25f174e4e3dbccfcb468b8123454b3c61ba94a7ae0a870905141b050ad94b8f" "ce784eb8135893a19e5553ed94cc694320b05228ce712a64b2fb69c0c54161b9" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "6108bcd711f66fcd71bf9707a6ba886d58465669bef7d919d6289ec729dc17ea" "2db9c83380f626b24a0ba7a1dd9972b72ec3e5ce9e58892350d7188106e0e114" default))
 '(package-selected-packages
   '(magithub elm-mode yaml-mode which-key use-package racket-mode markdown-mode magit json-mode helm-projectile helm-ag git-gutter flycheck-clj-kondo emmet-mode dracula-theme company ag))
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
