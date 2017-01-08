#!/bin/bash

dir=".configs"

sudo apt-get install tmux zsh git-core python-pip htop wget vim -y
git clone --recursive https://github.com/averrin/configs.git $dir
cd ~
ln -s $dir/tmux.conf .tmux.conf
ln -s $dir/gitconfig .gitconfig
ln -s $dir/tmux .tmux
ln -s $dir/vimrc .vimrc
ln -s $dir/vim .vim

curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
ln -s $dir/zshrc .zshrc
cd ~/.oh-my-zsh/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git

cd ~
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
pip install --user git+git://github.com/powerline/powerline
mkdir ~/.fonts
cd ~/.fonts
wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
cd ~
fc-cache -vf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d/
cd ~/.config/fontconfig/conf.d/
wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
cd

echo ""
echo "*********************************"
echo "* Done. type 'tmux -u' to start *"
echo "*********************************"
echo ""
