is_msys || return 0

# See: http://qiita.com/ass_out/items/1387b7dd40ec12c2025e
function _pecowrap_exec() {
    eval "$@" > /tmp/cmd.log
    script -e -qc "winpty peco /tmp/cmd.log" /tmp/scriptxx.log
}
function _pecowrap_result() {
    local result="$(col -bx < /tmp/scriptxx.log | tr -d '\n' | sed 's/.*0m\(.*\)0K.*$/\1/g' | sed 's/0K//g')"
    echo "${result}"
}

# リポジトリにcd
function peco-src () {
    _pecowrap_exec "ghq list -p"
    local selected_dir=$(_pecowrap_result)
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^j' peco-src
