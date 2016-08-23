# 文字列操作
function downcase() { tr "[:upper:]" "[:lower:]"; }
function upcase() { tr "[:lower:]" "[:upper:]"; }

# OS判定
function ostype() { uname| downcase; }
function is_linux() { [[ `ostype` == linux* ]]; }
function is_ubuntu() { grep 'DISTRIB_ID=Ubunt' /etc/lsb-release > /dev/null 2>&1 ; }
function is_osx() { [[ `ostype` == darwin* ]]; }
function is_bsd() { [[ `ostype` == bsd* ]]; }
function is_msys() { [[ `uname` == MSYS* ]]; }  # workaround: ostypeが空文字列返す時がある...

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

function die() {
    logError "$1" 1>&2
    exit ${2:-1}
}

## ディレクトリやシンボリックリンク実体化を含め上書きコピー
function cpDeep() { cp -rfH $* ; }

## アーカイブファイルの展開
function extract() {
    local srcArchive=$1
    local destDir=$2
    (
        cd $destDir
        if [[ $srcArchive == *.tar.gz ]]; then
            tar xf $srcArchive
        elif [[ $srcArchive == *.zip ]]; then
            unzip $srcArchive
        fi
    )
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
