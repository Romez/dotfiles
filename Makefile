zsh:
	ansible-playbook zsh.yml

zsh-config:
	ansible-playbook zsh.yml --tags config

dotfiles:
	ansible-playbook dotfiles.yml

emacs:
	ansible-playbook emacs.yml

tmux-conf:
	ansible-playbook tmux.yml --tags conf
