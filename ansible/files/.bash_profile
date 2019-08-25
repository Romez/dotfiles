alias l='ls -laG'
alias ll='ls -laG | fzf'
alias cl='clear'

export NVM_DIR="$HOME/.nvm"
source $(brew --prefix nvm)/nvm.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias yarn='docker run -it --rm -v `pwd`:/pwd -w /pwd node yarn'
# alias nvim='docker run -v `pwd`:/src -it romezzz/nvim nvim'

export FZF_DEFAULT_COMMAND='ag --path-to-ignore ~/.ignore --hidden -l -g ""'
