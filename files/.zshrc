export NVM_DIR="$HOME/.nvm"

# source $(brew --prefix nvm)/nvm.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

plugins=(git nvm npm mvn docker docker-compose kubectl dotenv tmux zsh-syntax-highlighting)

ZSH_THEME="dracula"

export ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

export EDITOR='emacs'

alias cl='clear'
alias python=/usr/local/bin/python3

alias k='kubectl'
alias mk='/usr/local/bin/minikube'
alias m='make'

export FZF_DEFAULT_COMMAND='ag --path-to-ignore ~/.ignore --hidden -l -g ""'

# set locale
export LC_ALL=en_US.UTF-8

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc' ]; then . '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc' ]; then . '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'; fi

autoload -Uz compinit
compinit
