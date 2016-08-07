is_linux || return 0

alias ls='ls --color'
alias ll='ls -l --color'
alias la='ls -a --color'
alias lla='ls -la --color'
alias l='ls -F --color'

function open() { gvfs-open $* 2> /dev/null }
