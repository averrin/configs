ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git git-flow tmux extract vi-mode zsh-syntax-highlighting z fzf-z history-substring-search command-not-found common-aliases)
source $ZSH/oh-my-zsh.sh
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

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
export FZF_DEFAULT_COMMAND='ag --ignore .git --ignore-dir node_modules -g ""'

export FZF_COMPLETION_OPTS='+c -x'

_fzf_compgen_path() {
  ag -g "" "$1"
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey -s '\ec' '$(find . -type d -not -path "*/\.*" -not -path "*/node_modules*" | fzf --read0 -0 -1)\n'

alias !="sudo"
alias pipi="sudo pip install"
alias ipython="ptipython"
alias commit="git commit -am"
alias tag="blight tag"
alias push="git push"
alias pull="git pull"
alias st="git status"
alias dff='git diff --color | diff-so-fancy'

