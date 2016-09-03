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
export PATH=$HOME/.jdk/jdk1.8.0_101/bin:$PATH

# scala
export PATH=$HOME/.scalaenv/bin:$PATH
eval "$(scalaenv init -)"

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
