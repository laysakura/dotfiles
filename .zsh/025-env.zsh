# HOME PATH
mkdir -p $HOME/.bin
export PATH=$HOME/.bin:$PATH

# homebrew
export PATH=$BREWPATH/bin:$PATH
export MANPATH=$BREWPATH/share/man:$MANPATH
export INFOPATH=$BREWPATH/share/info:$INFOPATH

# golang
export GOROOT=$HOME/.go
export GOPATH=$HOME/.ghq
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH
