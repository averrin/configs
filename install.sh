#!/bin/bash

dir=".configs"
st_version="0.7"
spacemacs_branch="develop"

sudo apt-get install tmux zsh git-core python-pip htop wget vim emacs rofi -y

rm -rf $dir
git clone --recursive https://github.com/averrin/configs.git $dir
cd ~

rm -rf .gitconfig
ln -s $dir/gitconfig .gitconfig

## Emacs
rm -rf ./.emacs.d
rm -rf ./.spacemacs
rm -rf ./.spacemacs.d
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
cd ~/.emacs.d
git checkout $spacemacs_branch
cd ~
ln -s $dir/spacemacs.d .spacemacs.d

## ZSH
cd ~
rm -rf ~/.oh-my-zsh
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
ln -s $dir/zshrc .zshrc
cd ~/.oh-my-zsh/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git
cd ~

cd ~
rm -rf ./.dircolors.256dark
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark .dircolors.256dark
pip install --user git+git://github.com/powerline/powerline
mkdir ~/.fonts
cd ~/.fonts
rm -rf PowerlineSymbols.otf
wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf

rm -rf FiraCode-*
wget https://github.com/tonsky/FiraCode/raw/master/distr/otf/FiraCode-Bold.otf -O ~/.fonts/FiraCode-Bold.otf
wget https://github.com/tonsky/FiraCode/raw/master/distr/otf/FiraCode-Light.otf -O ~/.fonts/FiraCode-Light.otf
wget https://github.com/tonsky/FiraCode/raw/master/distr/otf/FiraCode-Medium.otf -O ~/.fonts/FiraCode-Medium.otf
wget https://github.com/tonsky/FiraCode/raw/master/distr/otf/FiraCode-Regular.otf -O ~/.fonts/FiraCode-Regular.otf
wget https://github.com/tonsky/FiraCode/raw/master/distr/otf/FiraCode-Retina.otf -O ~/.fonts/FiraCode-Retina.otf
cd ~
fc-cache -vf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d/
cd ~/.config/fontconfig/conf.d/
wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf
cd ~

## Tmux
cd ~
ln -s $dir/tmux.conf .tmux.conf
ln -s $dir/tmux .tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
cd ~

## St
cd $dir
wget http://dl.suckless.org/st/st-0.7.tar.gz
tar -xf st-$st_version.tar.gz
cd ./st-$st_version
rm -rf config.h
ln -s ../st-config.h config.h
make
sudo make install
cd ~

## Rofi
cd ~
rm -rf .XResources
ln -s $dir/XResources .XResources
cd ~

echo ""
echo "*********************************"
echo "* Done. type 'tmux -u' to start *"
echo "*********************************"
echo ""
