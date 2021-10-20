<<<<<<< HEAD:.zsh/000-zplug.zsh
source /usr/local/opt/zplug/init.zsh || source /usr/share/zplug/init.zsh || ~/.zplug/init.zsh
=======
if is_osx; then
  source /usr/local/opt/zplug/init.zsh
elif is_linux; then
  source /usr/share/zplug/init.zsh
fi
>>>>>>> c6f5464bec8f983ae5ddbafcb276861d5b1b9d56:.zsh/001-zplug.zsh

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Then, source plugins and add commands to $PATH
zplug check || zplug install
zplug load --verbose
