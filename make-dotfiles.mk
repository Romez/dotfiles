vimrc:
	ansible-playbook ansible/dotfiles.yml -i ansible/inventory -vv --tags "vimrc"
dotfiles:
	ansible-playbook ansible/dotfiles.yml -i ansible/inventory -vv
