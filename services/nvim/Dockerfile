FROM ubuntu:18.04

RUN  apt-get update && apt-get install -y \
software-properties-common \
curl \
git \
python3-pip

#install pynvim for deoplete
RUN pip3 install --user pynvim

#install neovim
RUN add-apt-repository -y ppa:neovim-ppa/stable \
&& apt-get update \
&& apt-get install -y \
neovim

#install vim-plug
RUN curl -LSso /root/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#install nodejs
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash - \
    && apt-get update && apt-get install -y nodejs

#copy vimrc
COPY .vimrc /root/.config/nvim/init.vim

#install npm global packages
RUN npm i -g \
typescript \
typescript-language-server \
css-language-server \
vscode-json-languageservice

#install plugs
RUN nvim +'PlugInstall --sync' +qa

WORKDIR /src

