ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git, autoenv, django,tmux, extract, zsh-syntax-highlighting, history-substring-search)
source $ZSH/oh-my-zsh.sh
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

alias !="sudo"
alias p="sudo python ~/p.py"
alias pipi="sudo pip install"

bindkey "\e[A" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down


eval `dircolors ~/dircolors.256dark`
