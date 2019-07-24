install-neovim:
	ansible-playbook ansible/dotfiles.yml -i ansible/inventory -vv --tags "neovim"
