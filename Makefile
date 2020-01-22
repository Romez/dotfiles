vim:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "vim" --ask-vault-pass

dotfiles:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "dotfiles" --ask-vault-pass

desktop:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "desktop"

cmd:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "cmd" --ask-vault-pass
