export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color
export EDITOR="emacs -nw"

export FZF_DEFAULT_COMMAND='ag --ignore .git --ignore-dir node_modules -g ""'
export FZF_COMPLETION_OPTS='+c -x'

export DART_SDK_PATH=/usr/local/opt/dart/libexec
export OVERRIDE_WRIKE_DART_DEPS_BRANCH=true
export DART_FLAGS="--checked"
export PUB_HOSTED_URL=http://pub-dev.wrke.in

export GOPATH=/Users/alexey.nabrodov/Projects/go;

export PATH="$PATH":"$GOPATH/bin"
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/.cargo/bin"
export PATH="$PATH":"$HOME/Projects/powerline/scripts"
export PATH="$PATH":"$HOME/.local/bin"
export PATH="$PATH":"$HOME/.local/share/flutter/bin"
