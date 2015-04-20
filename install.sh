sudo apt-get install tmux zsh git-core python-pip htop wget tig vim -y
sudo pip install jedi
sudo pip install ipython
sudo pip install percol
sudo pip install prompt-toolkit
sudo pip install thefuck
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
git clone git://github.com/kennethreitz/autoenv.git ~/.autoenv
git clone https://github.com/mhinz/vim-startify ~/.vim/bundle/vim-startify
echo "alias averrin='tmux -u attach -t averrin || tmux -u new -s averrin'" >> ~/.bashrc
echo "alias tester='tmux -u attach -t tester || tmux -u new -s tester'" >> ~/.bashrc
cd
wget https://raw.github.com/seebi/dircolors-solarized/master/dircolors.256dark
echo "Done. type 'tmux -u' to start"
