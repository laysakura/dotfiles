is_osx || return 0

# pyenv init must run after all PATH additions (025-env.zsh etc.)
# so that pyenv shims take priority over Homebrew's python
eval "$(pyenv init -)"
