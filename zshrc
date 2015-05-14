ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git, git-flow, autoenv, django,tmux, extract, zsh-syntax-highlighting, history-substring-search)
source $ZSH/oh-my-zsh.sh
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

alias !="sudo"
alias p="sudo python ~/p.py"
alias pipi="sudo pip install"
alias ipython="ptipython"
alias ipy="ipython"
alias ch="sudo python ~/projects/ch.py"
alias how="/home/user/.local/bin/howdoi"
alias commit="git commit -am"
alias push="git push"
alias status="git status"
alias build="gulp build"
alias fuck='$(thefuck $(fc -ln -1))'

bindkey "\e[A" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down


eval `dircolors ~/dircolors.256dark`

function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi
export TERM=xterm-256color
export PATH=$PATH:~/.local/bin

