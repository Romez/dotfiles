alias l='ls -laG'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias yarn='docker run -it --rm -v `pwd`:/pwd -w /pwd node yarn'

export FZF_DEFAULT_OPTS="--ansi --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"

alias myvim='docker run -it -v $HOME/.config/nvim:/root/.config/nvim -v $HOME/.vimrc:/root/.vimrc -v `pwd`:/data --name neovim romezzz/neovim'
