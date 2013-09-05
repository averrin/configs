ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git, zsh-syntax-highlighting, history-substring-search)
source $ZSH/oh-my-zsh.sh
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
#source /usr/share/autojump/autojump.sh

#PS1="[%{$fg[cyan]%}$TMUX_PANE%{$reset_color%}]$PS1"


alias !="sudo"
alias p="sudo python ~/p.py"
alias pipi="sudo pip install"

zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
