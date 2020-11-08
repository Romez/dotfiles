nvim:
	ansible-playbook nvim.yml 

zsh:
	ansible-playbook zsh.yml

dotfiles:
	ansible-playbook dotfiles.yml

desktop:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "desktop"

cmd:
	ansible-playbook cmd.yml
