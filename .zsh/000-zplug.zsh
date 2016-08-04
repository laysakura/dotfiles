source ~/.zplug/init.zsh

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting", nice:10

# Then, source plugins and add commands to $PATH
zplug load --verbose
