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
  :config (helm-projectile-on))

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
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(setq backup-directory-alist `(("." . "~/.saves")))

(setq inhibit-startup-message t)

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode 1)

;; increase messages buffer sieze
(setq message-log-max 20000)

(defun cider-reset ()
  (interactive)
  (cider-interactive-eval "(require 'dev)(dev/reset)"))

(global-set-key (kbd "C-x C-j") 'cider-reset)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)

;;; .emacs ends here
