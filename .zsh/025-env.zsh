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
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.bin:$PATH
export PATH=$HOME/usr/local/bin:$PATH
export PATH=$HOME/usr/local/sbin:$PATH
export PATH=$HOME/usr/bin:$PATH
export PATH=$HOME/usr/sbin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH

# Language
export LC_ALL=ja_JP.UTF-8

# less
export LESS='-i -M -R -W -x4'

# rust
export RUST_SRC_PATH=$HOME/.ghq/src/github.com/rust-lang/rust/src

# poetry
export PATH=$HOME/.poetry/bin:$PATH

# emacs cask
export PATH=$HOME/.cask/bin:$PATH

# rust
test -e $HOME/.cargo/env && . $HOME/.cargo/env

# ruby
export PATH="$HOME/.rbenv/bin:$PATH"
has rbenv && eval "$(rbenv init -)"

# golang
[ "$GOROOT" ]
export GOPATH=$HOME/.ghq
export PATH=$GOROOT/bin:$GOPATH/bin:$PATH

# node-js
export PATH=$HOME/.nodebrew/current/bin:$PATH
