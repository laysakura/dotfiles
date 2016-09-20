#zsh
## Regard `/` as word splitter (for M-f, M-b, ...)
export WORDCHARS="*?_-.[]~&;=!#$%^(){}<>"
## Save histories
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=1000
export SAVEHIST=100000

# editor
export EDITOR=vim  # just for speed..

# HOME PATH
export PATH=$HOME/.bin:$PATH

# java
export PATH=$JAVA_HOME/bin:$PATH

# scala
export PATH=$HOME/.scalaenv/bin:$PATH
eval "$(scalaenv init -)"

# flyway
export PATH=$HOME/.flyway:$PATH

# ruby
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# golang
export GOROOT=$HOME/.go
export GOPATH=$HOME/.ghq
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# node-js
export PATH=$HOME/.nodebrew/current/bin:$PATH

# kdb+
export QHOME=$HOME/.q
export PATH=$QHOME/l32:$PATH
