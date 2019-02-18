export KDEWM=i3
export TERM=xterm-256color
export EDITOR="emacs -nw"

export FZF_DEFAULT_COMMAND='ag --ignore .git --ignore-dir node_modules -g ""'
export FZF_COMPLETION_OPTS='+c -x'

export DART_SDK_PATH=/usr/lib/dart
export OVERRIDE_WRIKE_DART_DEPS_BRANCH=true
export DART_FLAGS="--checked"
export PUB_HOSTED_URL=http://pub-dev.wrke.in

export GOPATH=/home/alexeynabrodov/go;

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/bin/dart-sdk/bin:/home/alexeynabrodov/.nvm/versions/node/v9.10.1/bin/:$GOPATH/bin:~/.local/bin:/usr/local/go/bin:/media/vault/stash/bin:~/projects/swift/usr/bin:$DART_SDK_PATH/bin
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/.cargo/bin"
export LANG=en_US.UTF-8
export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
