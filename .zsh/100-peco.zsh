# ヒストリ補完
function pecoHistorySelection() {
    if has tac; then
        tac='tac'
    else
        tac='tail -r'
    fi
    BUFFER=$(history -n 1 | eval $tac  | awk '!a[$0]++' | peco)
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N pecoHistorySelection

# リポジトリにcd
function pecoCdRepo () {
    local selectedDir=$(ghq list -p | peco --query "$LBUFFER")
    if [ -n "$selectedDir" ]; then
        BUFFER="cd ${selectedDir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N pecoCdRepo
