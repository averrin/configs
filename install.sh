sudo apt-get install tmux zsh git-core python-pip
sudo pip install jedi
sudo pip install ipython
git clone --recursive https://github.com/averrin/maximum-awesome.git
cd ./maximum-awesome
cp -r ./tmux.conf ~/.tmux.conf
cp ./zshrc ~/.zshrc
cp ./vimrc ~/.vimrc
cp -r vim ~/.vim
cp -r ~/.vim/vim/* ~/.vim
cp -r ./oh-my-zsh ~/.oh-my-zsh
cp ./zsh-history-substring-search/* ~/.oh-my-zsh/custom/
echo "Done. type 'tmux -u' to start"
