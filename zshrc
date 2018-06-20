ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(extract vi-mode zsh-syntax-highlighting z common-aliases history-substring-search command-not-found nvm)
source $ZSH/oh-my-zsh.sh

bindkey "\e[A" history-substring-search-up
bindkey "^p" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "^n" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down

eval `dircolors ~/.dircolors.256dark`
. ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

chf() {
  local branches branch
  branches=$(git for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf --height 40% --query="$1" +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

. /home/alexey.nabrodov@team.wrike.com/.nix-profile/etc/profile.d/nix.sh

alias !="sudo"
alias ls="exa --color=always"
alias la="exa -la --color=always"
alias cmt="git commit -am"
alias push="git push"
alias pull="git pull"
alias st="git status --short --branch || ls"
alias dff='git diff --color | diff-so-fancy | less'

alias ch="git checkout"
alias master="git checkout master"
alias e.="nohup emacs . >/dev/null 2>&1 &"
alias ec="emacs -nw"
alias x="xdg-open"

alias task="blight openTask"
alias task_link="blight openTask -c"
alias grep="grep -P"
alias less="less -R"

commit() {
    blight commit "$@";
}

branch() {
  if [ `git rev-parse --verify $1` ]
  then
    git checkout $1
  else
    git checkout -b $1; ec ./CHANGELOG.md; git add ./CHANGELOG.md; git commit -m 'create branch'; git push --set-upstream origin $1;
  fi
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
alias merge="git checkout master; git pull; git checkout -; git merge master --no-ff"

export KEYTIMEOUT=1
. ~/.zshenv
#. ~/projects/icons-in-terminal/build/icons_bash_export.sh

PATH=$PATH:/home/alexeynabrodov/.local/share;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 885C7A40-D2F8-9B76-A9D2-25172514BFBA
