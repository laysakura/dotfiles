#zsh
## Regard `/` as word splitter (for M-f, M-b, ...)
export WORDCHARS="*?_-.[]~&;=!#$%^(){}<>"
## Save histories
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=100000
export SAVEHIST=100000

# editor
export EDITOR=emacs

# HOME PATH
export PATH=$HOME/.bin:$PATH
export PATH=$HOME/usr/local/bin:$PATH
export PATH=$HOME/usr/local/sbin:$PATH
export PATH=$HOME/usr/bin:$PATH
export PATH=$HOME/usr/sbin:$PATH

# Language
export LC_ALL=ja_JP.UTF-8

# less
export LESS='-i -M -R -W -x4'

# rust
export RUST_SRC_PATH=$HOME/.ghq/src/github.com/rust-lang/rust/src

# java
export PATH=$JAVA_HOME/bin:$PATH

# scala
export PATH=$HOME/.scalaenv/bin:$PATH
eval "$(scalaenv init -)"
export SBT_OPTS="-Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M"

# flyway
export PATH=$HOME/.flyway:$PATH

# rust
test -e $HOME/.cargo/env && . $HOME/.cargo/env

# ruby
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# golang
export GOROOT=$HOME/usr/local/opt/go/libexec
export GOPATH=$HOME/.ghq
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# node-js
export PATH=$HOME/.nodebrew/current/bin:$PATH

# kdb+
export QHOME=$HOME/.q
export PATH=$QHOME/l32:$PATH

# direnv
eval "$(direnv hook zsh)"
