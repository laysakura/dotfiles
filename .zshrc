for f in $HOME/.zsh/[0-9]*.zsh; do . $f; done

# direnv
eval "$(direnv hook zsh)"
