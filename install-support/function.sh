# 文字列操作
function downcase() {
    tr "[:upper:]" "[:lower:]"
}
function upcase() {
    tr "[:lower:]" "[:upper:]"
}

# OS判定
function ostype() {
    uname| downcase
}
function is_linux() { [[ `ostype` == linux* ]]; }
function is_osx() { [[ `ostype` == darwin* ]]; }
function is_bsd() { [[ `ostype` == bsd* ]]; }
function is_msys() { [[ `ostype` == msys* ]]; }
