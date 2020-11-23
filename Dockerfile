FROM ubuntu

RUN apt update
RUN apt install -y neovim bat fzf grip silversearcher-ag curl git
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash - && apt install nodejs && npm install --global yarn

# && apt-get clean \
# && rm -rf /var/lib/apt/lists/*

RUN curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

COPY files/vimrc /root/.config/nvim/init.vim
COPY files/coc-settings.json /root/.config/nvim/

# RUN nvim -i NONE -c PlugInstall -c quitall
RUN nvim -i NONE -c PlugInstall -c quitall

RUN nvim -c 'CocInstall -sync \
  coc-explorer\
  # coc-clangd \
  # coc-css \
  # coc-db \
  # coc-docker \
  # coc-eslint \
  # coc-git \
  # coc-highlighta \
  # coc-prettier \
  # coc-python \
  # coc-solargraph \
  # coc-stylelint \
  # coc-stylelintplus \
  # coc-tsserver \
  # coc-yaml \
  |q'

WORKDIR /app

# ENTRYPOINT ["nvim"]
