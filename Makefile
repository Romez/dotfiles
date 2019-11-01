vim:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "vim"

dotfiles:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "dotfiles"

desktop:
	ansible-playbook ansible/main.yml -i ansible/inventory -vv --tags "desktop"
