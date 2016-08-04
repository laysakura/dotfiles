# 文字列操作
function downcase() {
    echo "$1" |tr "[:upper:]" "[:lower:]"
}
function upcase() {
    echo "$1" |tr "[:lower:]" "[:upper:]"
}

# OS判定
function ostype() {
    uname| downcase
}
function is_linux() { [[ `ostype` == linux* ]]; }
function is_osx() { [[ `ostype` == darwin* ]]; }
function is_bsd() { [[ `ostype` == bsd* ]]; }
function is_msys() { [[ `ostype` == msys* ]]; }
