alias emacs="emacs -nw"
alias be="bundle exec"
alias plantuml="java -jar $HOME/.plantuml.jar -charset UTF-8"
alias q="rlwrap -c -r q"
alias scala='scala -Dscala.color'
alias sbt='sbt -Dscala.color'
alias graphicsmagick='gm'
alias deleteLocalBranch="git branch --merged main | grep -vE '^\*|main$|develop/.+$' | xargs -I % git branch -d %"

mkd() {
	mkdir -p $1 && cd $1
}

# OSC 52を使い、第一引数または標準入力の文字列を、ssh先のホストのクリップボードにコピーする
# https://zenn.dev/anyakichi/articles/40d7464fdf0e31
tcopy() {
    if [ $# -eq 0 ]; then
        printf "\e]52;c;%s\a" "$(base64 -w 0)"
    else
        printf "\e]52;c;%s\a" "$(printf "%s" "$*" | base64 -w 0)"
    fi
}
