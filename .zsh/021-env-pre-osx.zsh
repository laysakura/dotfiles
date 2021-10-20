is_osx || return 0

# Homebrew
export BREWPATH=/opt/homebrew
export PATH=$BREWPATH/bin:$PATH
export PATH=$BREWPATH/sbin:$PATH
export MANPATH=$BREWPATH/share/man:${MANPATH:-}
export INFOPATH=$BREWPATH/share/info:${INFOPATH:-}
export HOMEBREW_CASK_OPTS="--require-sha --appdir=~/Applications"
## セキュリティ
export HOMEBREW_NO_INSECURE_REDIRECT=1
export HOMEBREW_NO_ANALYTICS=1

# ssh-agent
ssh-add -K ~/.ssh/id_rsa_github_nopass

# Python (pyenv is supposed to be installed via homebrew)
eval "$(pyenv init -)"
