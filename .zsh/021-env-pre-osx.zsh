is_osx || return 0

# Homebrew
export BREWPATH=$HOME/.homebrew
export PATH=$BREWPATH/bin:$PATH
export PATH=$BREWPATH/sbin:$PATH
export MANPATH=$BREWPATH/share/man:${MANPATH:-}
export INFOPATH=$BREWPATH/share/info:${INFOPATH:-}

# Java
export JAVA_HOME=`/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home`

# Python (pyenv is supposed to be installed via homebrew)
eval "$(pyenv init -)"
