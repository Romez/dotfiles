nvim:
	ansible-playbook nvim.yml

spacevim:
	ansible-playbook spacevim.yaml

zsh:
	ansible-playbook zsh.yml

zsh-config:
	ansible-playbook zsh.yml --tags config

dotfiles:
	ansible-playbook dotfiles.yml

desktop:
	ansible-playbook desktop.yml

cmd:
	ansible-playbook cmd.yml

emacs:
	ansible-playbook emacs.yml
