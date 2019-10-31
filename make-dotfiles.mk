vim:
	ansible-playbook ansible/dotfiles.yml -i ansible/inventory -vv --tags "vim"

dotfiles:
	ansible-playbook ansible/dotfiles.yml -i ansible/inventory -vv
