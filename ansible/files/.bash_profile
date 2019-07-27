alias l='ls -laG'

export NVM_DIR="$HOME/.nvm"
source $(brew --prefix nvm)/nvm.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias yarn='docker run -it --rm -v `pwd`:/pwd -w /pwd node yarn'
alias nvim='docker run --volume `pwd`:/src -it romezzz/nvim nvim'

export FZF_DEFAULT_OPTS="--ansi --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
export FZF_DEFAULT_COMMAND='ag --path-to-ignore ~/.ignore --hidden -g ""'
