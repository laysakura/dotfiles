alias emacs="emacs -nw"
alias be="bundle exec"
alias plantuml="java -jar $HOME/.plantuml.jar -charset UTF-8"
alias q="rlwrap -c -r q"
alias scala='scala -Dscala.color'
alias sbt='sbt -Dscala.color'
alias graphicsmagick='gm'
alias deleteLocalBranch="git branch --merged main | grep -vE '^\*|main$|develop/.+$' | xargs -I % git branch -d %"
