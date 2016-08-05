# 文字列操作
function downcase() { tr "[:upper:]" "[:lower:]"; }
function upcase() { tr "[:lower:]" "[:upper:]"; }

# OS判定
function ostype() { uname| downcase; }
function is_linux() { [[ `ostype` == linux* ]]; }
function is_osx() { [[ `ostype` == darwin* ]]; }
function is_bsd() { [[ `ostype` == bsd* ]]; }
function is_msys() { [[ `ostype` == msys* ]]; }

# logger
function logError() {
    printf "\033[31m%s\033[m\n" "[ERROR] $*" 1>&2
}
function logWarn() {
    printf "\033[33m%s\033[m\n" "[WARN]  $*" 1>&2
}
function logInfo() {
    printf "\033[36m%s\033[m\n" "[INFO]  $*" 1>&2
}
function logOk() {
    printf "\033[32m%s\033[m\n" "[OK]    $*" 1>&2
}

# シェル
function has() {
    type "$1" >/dev/null 2>&1
    return $?
}

function isInteractiveShell() { [ ! -z "$PS1" ]; }
function isSshRunning() { [ ! -z "$SSH_CONECTION" ]; }

## ディレクトリやシンボリックリンク実体化を含め上書きコピー
function cpDeep() { cp -rfH $* ; }

function die() {
    logError "$1" 1>&2
    exit ${2:-1}
}

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
