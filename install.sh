#!/bin/bash
#TODO: install for st
#TODO: settings for rofi

dir=".configs"

sudo apt-get install tmux zsh git-core python-pip htop wget vim emacs rofi -y
git clone --recursive https://github.com/averrin/configs.git $dir
cd ~

ln -s $dir/gitconfig .gitconfig

## Emacs
rm -rf ./.emacs.d
rm -rf ./.spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s $dir/spacemacs .spacemacs
bindir=$(dirname $(which vim))
cd $bindir
sudo mv vim vim_orig
sudo ln -s emacsclient vim

## ZSH
cd ~
curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
ln -s $dir/zshrc .zshrc
cd ~/.oh-my-zsh/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git

cd ~
rm -rf ./dircolors.256dark
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
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

## Tmux
ln -s $dir/tmux.conf .tmux.conf
ln -s $dir/tmux .tmux
rm -rf ~/.tmux/plugins/tpm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
cd

echo ""
echo "*********************************"
echo "* Done. type 'tmux -u' to start *"
echo "*********************************"
echo ""
