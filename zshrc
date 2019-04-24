ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
plugins=(extract vi-mode z history-substring-search command-not-found nvm)
source $ZSH/oh-my-zsh.sh

bindkey "\e[A" history-substring-search-up
bindkey "^p" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "^n" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down

#eval `dircolors ~/.dircolors.256dark`
. /Users/alexey.nabrodov/Library/Python/3.7/lib/python/site-packages/powerline/bindings/zsh/powerline.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

chf() {
  local branches branch
  branches=$(git for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf --height 40% --query="$1" +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

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
#alias e.="nohup emacs . >/dev/null 2>&1 &"
alias ec="emacs -nw"
alias x="open"

alias task="blight openTask"
alias task_link="blight openTask -c"
alias less="less -R"


# ls, the common ones I use a lot shortened for rapid fire usage
alias l='ls -lFh'     #size,show type,human readable
alias la='ls -lAFh'   #long list,show almost all,show type,human readable
alias lr='ls -tRFh'   #sorted by date,recursive,show type,human readable
alias lt='ls -ltFh'   #long list,sorted by date,show type,human readable
alias ll='ls -l'      #long list
alias ldot='ls -ld .*'
alias lS='ls -1FSsh'
alias lart='ls -1Fcart'
alias lrt='ls -1Fcrt'

alias zshrc='${=EDITOR} ~/.zshrc' # Quick access to the ~/.zshrc file

alias grep='grep --color'
alias sgrep='grep -R -n -H -C 5 --exclude-dir={.git,.svn,CVS} '

alias t='tail -f'

# Command line head / tail shortcuts
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g L="| less"
alias -g M="| most"
alias -g LL="2>&1 | less"
alias -g CA="2>&1 | cat -A"
alias -g NE="2> /dev/null"
alias -g NUL="> /dev/null 2>&1"
alias -g P="2>&1| pygmentize -l pytb"

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

commit() {
    blight commit "$@";
}

branch() {
  if [ `git rev-parse --verify $1` ]
  then
    git checkout $1
  else
    git checkout -b $1;
    # TODO: move to blight for title fetching
    change="MINOR\n- [https://www.wrike.com/open.htm?id=$(git branch | grep \* | cut -d ' ' -f2 | cut -d '-' -f1)]"
    original=$(cat ./CHANGELOG.md)
    echo -e "$change\n" > ./CHANGELOG.md
    echo $original >> ./CHANGELOG.md
    ec ./CHANGELOG.md;
    git add ./CHANGELOG.md;
    git commit -m 'create branch';
    git push --no-verify --set-upstream origin $1;
  fi
}

tag() {
  git checkout master;
  git pull;
  git tag $1;
  git push --tags;
}

alias stash="git stash"
alias pop="git stash pop"
alias merge="git checkout master; git pull; git checkout -; git merge master --no-ff"

export KEYTIMEOUT=1
. ~/.zshenv
#. ~/projects/icons-in-terminal/build/icons_bash_export.sh

PATH=$PATH:/home/alexeynabrodov/.local/share;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 885C7A40-D2F8-9B76-A9D2-25172514BFBA
