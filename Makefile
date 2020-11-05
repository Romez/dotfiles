nvim:
	ansible-playbook nvim.yml 

zsh:
	ansible-playbook zsh.yml

dotfiles:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "dotfiles" --ask-vault-pass

desktop:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "desktop"

cmd:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "cmd" --ask-vault-pass
