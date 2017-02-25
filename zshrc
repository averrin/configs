ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(git extract vi-mode zsh-syntax-highlighting z common-aliases history-substring-search command-not-found dirhistory nvm)
source $ZSH/oh-my-zsh.sh

bindkey "\e[A" history-substring-search-up
bindkey "^p" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "^n" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down

eval `dircolors ~/dircolors.256dark`
. /home/alexeynabrodov/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

alias !="sudo"
alias commit="git commit -am"
alias push="git push"
alias pull="git pull"
alias st="git status --short --branch"
alias dff='git diff --color | diff-so-fancy | less'

alias ch="git checkout"
alias master="git checkout master"
alias e.="nohup emacs . >/dev/null 2>&1 &"
alias vim="emacsclient -a vim"

branch() {
  git checkout -b $1; git push --set-upstream origin $1;
}

tag() {
  git checkout master;
  git pull;
  git tag $1;
  git push --tags;
}

e() {
    nohup emacs $1 >/dev/null 2>&1 &;
}

alias stash="git stash"
alias pop="git stash pop"
alias merge="git checkout master; git pull; git checkout -; git merge master"

export KEYTIMEOUT=1
