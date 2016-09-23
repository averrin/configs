sudo apt-get install tmux zsh git-core python-pip htop wget vim -y
git clone --recursive https://github.com/averrin/maximum-awesome.git
cd ./maximum-awesome
cp -r ./tmux.conf ~/.tmux.conf
cp ./vimrc ~/.vimrc
cp -r vim ~/.vim
cp -r ~/.vim/vim/* ~/.vim
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
cp ./zshrc ~/.zshrc
cd ~/.oh-my-zsh/plugins
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git
git clone https://github.com/mhinz/vim-startify ~/.vim/bundle/vim-startify
cd
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
echo "Done. type 'tmux -u' to start"
