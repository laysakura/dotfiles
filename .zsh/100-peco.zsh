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
