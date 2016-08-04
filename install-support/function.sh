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

# シェル
function isExist() { type "$1" >/dev/null 2>&1; return $?; }
function isInteractiveShell() { [ ! -z "$PS1" ]; }
function isSshRunning() { [ ! -z "$SSH_CONECTION" ]; }

# git操作
## git管理されているファイルを丸っとコピー
function gitCopy() {
    local srcRepoDir=$1
    local destDir=$2
    local treeIsh=$3
    local tarFile=`mktemp`.tar
    (
        cd $srcRepoDir
        git archive -o $tarFile $treeIsh
        cd $destDir
        tar xf $tarFile
    )
}
