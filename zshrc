# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sporty_256"
ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(extract vi-mode z history-substring-search command-not-found nvm)
source $ZSH/oh-my-zsh.sh

bindkey "\e[A" history-substring-search-up
bindkey "^p" history-substring-search-up
bindkey "\e[B" history-substring-search-down
bindkey "^n" history-substring-search-down
bindkey "\eOA" history-substring-search-up
bindkey "\eOB" history-substring-search-down

#eval `dircolors ~/.dircolors.256dark`
#source "/usr/lib/python3.7/site-packages/powerline/bindings/zsh/powerline.zsh"

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
alias commit="git commit -am"
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

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

ZLE_RPROMPT_INDENT=0
