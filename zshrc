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

export DART_SDK_PATH=~/bin/dart-sdk
export OVERRIDE_WRIKE_DART_DEPS_BRANCH=true
export DART_FLAGS="--checked"
export PUB_HOSTED_URL=http://pub-dev.wrke.in
PATH=${PATH}:~/bin/dart-sdk/bin
export NVM_DIR="/home/alexeynabrodov/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
export GOPATH=/home/alexeynabrodov/go;
export PATH=$PATH:/home/alexeynabrodov/.nvm/versions/node/v5.12.0/bin/:$GOPATH/bin;

. /home/alexeynabrodov/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

alias !="sudo"
alias pipi="sudo pip install"
alias ipython="ptipython"
alias commit="git commit -am"
alias tag="blight tag"
alias push="git push"
alias pull="git pull"
alias st="git status"
alias dff='git diff --color | diff-so-fancy | less'

alias ch="git checkout"
alias master="git checkout master"
alias branch="git checkout -b"
alias stash="git stash"
alias pop="git stash pop"
alias merge="git checkout master; git pull; git checkout -; git merge master"

export KEYTIMEOUT=1
