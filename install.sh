sudo apt-get install tmux zsh git-core python-pip htop wget vim -y
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
git clone --recursive https://github.com/averrin/maximum-awesome.git
cd ./maximum-awesome
cp -r ./tmux.conf ~/.tmux.conf
cp ./zshrc ~/.zshrc
cp ./vimrc ~/.vimrc
cp -r vim ~/.vim
cp -r ~/.vim/vim/* ~/.vim
cp -r ./oh-my-zsh ~/.oh-my-zsh
cd ~/.oh-my-zsh/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git
git clone https://github.com/mhinz/vim-startify ~/.vim/bundle/vim-startify
cd
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
echo "Done. type 'tmux -u' to start"
