is_osx || return 0

# Homebrew
export BREWPATH=$HOME/.homebrew
export PATH=$BREWPATH/bin:$PATH
export MANPATH=$BREWPATH/share/man:${MANPATH:-}
export INFOPATH=$BREWPATH/share/info:${INFOPATH:-}
