#source "$HOME/.antigen/antigen.zsh"

#antigen-use oh-my-zsh
#antigen-bundle arialdomartini/oh-my-git
#antigen theme arialdomartini/oh-my-git-themes oppa-lana-style

#antigen-apply


ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git git-flow tmux extract vi-mode zsh-syntax-highlighting z fzf-z history-substring-search)
source $ZSH/oh-my-zsh.sh
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

alias !="sudo"
alias pipi="sudo pip install"
alias ipython="ptipython"
alias commit="git commit -am"
alias tag="blight tag"
alias push="git push"
alias pull="git pull"
alias st="git status"
alias dff='git diff --color | diff-so-fancy'

bindkey "\e[A" history-substring-search-up
bindkey "^p" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "^n" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down

cf() {
  local file

  file="$(locate -Ai -0 $@ | grep -z -vE '~$' | fzf --read0 -0 -1)"

  if [[ -n $file ]]
  then
     if [[ -d $file ]]
     then
        cd -- $file
     else
        cd -- ${file:h}
     fi
  fi
}
co () {
  git commit -am "'$*'"
}

eval `dircolors ~/dircolors.256dark`

export TERM=xterm-256color
export PATH=$PATH:~/.local/bin:/usr/local/go/bin:/home/user/projects/go/bin
export EDITOR=vim
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore-dir node_modules -g ""'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey -s '\ec' 'ed\n'
