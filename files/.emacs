;;; .emacs --- My custom config

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
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-s") 'helm-occur))

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

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  (setq helm-follow-mode-persistent t))

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

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

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

(use-package origami
  :ensure t
  :config
  (define-key global-map (kbd "C-x C-z") 'origami-mode-map)
  (define-prefix-command 'origami-mode-map)
  (define-key origami-mode-map (kbd "o") 'origami-open-node)
  (define-key origami-mode-map (kbd "O") 'origami-open-node-recursively)
  (define-key origami-mode-map (kbd "c") 'origami-close-node)
  (define-key origami-mode-map (kbd "C") 'origami-close-node-recursively)
  (define-key origami-mode-map (kbd "a") 'origami-toggle-node)
  (define-key origami-mode-map (kbd "A") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "R") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "M") 'origami-close-all-nodes)
  (define-key origami-mode-map (kbd "v") 'origami-show-only-node)
  (define-key origami-mode-map (kbd "k") 'origami-previous-fold)
  (define-key origami-mode-map (kbd "j") 'origami-forward-fold)
  (define-key origami-mode-map (kbd "x") 'origami-reset)  
  )

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

(setq show-trailing-whitespace t)

(defun cider-reset ()
  (interactive)
  (cider-interactive-eval "(require 'dev)(dev/reset)"))

(global-set-key (kbd "C-x C-j") 'cider-reset)

(global-prettify-symbols-mode 1)

(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elm-mode yaml-mode which-key use-package racket-mode markdown-mode magit json-mode helm-projectile helm-ag git-gutter flycheck-clj-kondo emmet-mode dracula-theme company clj-refactor ag))
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
