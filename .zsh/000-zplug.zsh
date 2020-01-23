source /usr/local/opt/zplug/init.zsh

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Then, source plugins and add commands to $PATH
zplug check || zplug install
zplug load --verbose
