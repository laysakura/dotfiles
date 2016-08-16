#zsh
## Regard `/` as word splitter (for M-f, M-b, ...)
export WORDCHARS="*?_-.[]~&;=!#$%^(){}<>"
## Save histories
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=1000
export SAVEHIST=100000

# HOME PATH
export PATH=$HOME/.bin:$PATH

# golang
export GOROOT=$HOME/.go
export GOPATH=$HOME/.ghq
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# node-js
export PATH=$HOME/.nodebrew/current/bin:$PATH
