sudo apt-get install tmux zsh git-core
get clone https://github.com/averrin/maximum-awesome.git
cd ./maximum-awesome
cp -r ./tmux.conf ~/.tmux.conf
cp ./zshrc ~/.zshrc
cp ./vimrc ~/.vimrc
cp -r vim ~/.vim
cp -r ~/.vim/vim/* ~/.vim
cp -r ./oh-my-zsh ~/.oh-my-zsh
tmux -u
