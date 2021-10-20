if is_osx; then
  source ~/.zplug/init.zsh
elif is_linux; then
  source /usr/share/zplug/init.zsh
fi

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Then, source plugins and add commands to $PATH
zplug check || zplug install
zplug load --verbose
