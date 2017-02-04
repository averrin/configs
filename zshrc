ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git git-flow extract vi-mode zsh-syntax-highlighting z fzf-z history-substring-search command-not-found dirhistory nvm)
source $ZSH/oh-my-zsh.sh

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


_fzf_compgen_path() {
  ag -g "" "$1"
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
bindkey -s '\ec' '$(find . -type d -not -path "*/\.*" -not -path "*/node_modules*" | fzf --read0 -0 -1)\n'

. /home/alexeynabrodov/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

alias !="sudo"
alias pipi="sudo pip install"
alias ipython="ptipython"
alias commit="git commit -am"
alias tag="blight tag"
alias push="git push"
alias pull="git pull"
alias st="git status --short --branch"
alias dff='git diff --color | diff-so-fancy | less'

alias ch="git checkout"
alias master="git checkout master"
alias t="z"

branch() {
  git checkout -b $1; git push --set-upstream origin $1;
}

alias stash="git stash"
alias pop="git stash pop"
alias merge="git checkout master; git pull; git checkout -; git merge master"

export KEYTIMEOUT=1
