#!/bin/bash -xeu

# 設定
declare -a DEPENDENCIES=(
    'git'
    'ruby'
)

declare -A SITE_URL=(
    [bitbucket]='bitbucket.org'
    [github]='github.com'
)

declare -A SITE_USER=(
    [bitbucket]='laysakura'
    [github]='laysakura'
)

declare -A REPO_DOTFILES_SECRET=(
    [site]='bitbucket'
    [name]='laysakura/dotfiles-secret'
)

declare -A REPO_DOTFILES=(
    [site]='github'
    [name]='laysakura/dotfiles'
)

HOME_BIN=$HOME/.bin

# dotfilesリポジトリをcloneするまでに使用する最低限の関数定義
function repoUrl() {
    local repoSite=$1
    local repoName=$2
    echo -n "https://${SITE_USER[$repoSite]}@${SITE_URL[$repoSite]}/$repoName.git"
}
function has() {
    type "$1" >/dev/null 2>&1
    return $?
}
function logError() {
    printf "\033[31m%s\033[m\n" "[ERROR] $*" 1>&2
}
function logInfo() {
    printf "\033[36m%s\033[m\n" "[INFO]  $*" 1>&2
}
function die() {
    logError "$1" 1>&2
    exit ${2:-1}
}

# インストール実行
function run() {
    local tmpdir=`mktemp -d`

    has git || die "You must have git command"

    # dotfilesリポジトリを探す
    # ローカルに存在していてghq管理されていたらそれを使う
    local localDotfilesRepo=`has ghq &&  ghq list --full-path --exact ${REPO_DOTFILES[name]}`
    if [ -z $localDotfilesRepo ]; then
        local dotfilesDir=$tmpdir/${REPO_DOTFILES[name]}
        git clone $(repoUrl ${REPO_DOTFILES[site]} ${REPO_DOTFILES[name]}) $dotfilesDir
    else
        logInfo "Using $localDotfilesRepo"
        local dotfilesDir=$localDotfilesRepo
    fi

    # dotfiles-secretリポジトリを探す
    # ローカルに存在していてghq管理されていたらそれを使う
    local localDotfilesSecretRepo=`has ghq &&  ghq list --full-path --exact ${REPO_DOTFILES_SECRET[name]}`
    if [ -z $localDotfilesSecretRepo ]; then
        local dotfilesSecretDir=$tmpdir/${REPO_DOTFILES_SECRET[name]}
        git clone $(repoUrl ${REPO_DOTFILES_SECRET[site]} ${REPO_DOTFILES_SECRET[name]}) $dotfilesSecretDir
    else
        logInfo "Using $localDotfilesSecretRepo"
        local dotfilesSecretDir=$localDotfilesSecretRepo
    fi

    # 補助関数が手に入ったのでsource
    local supportDir=$dotfilesDir/install-support
    . $supportDir/function.sh
    logOk "source-ed $supportDir/function.sh"

    # dotfiles-secretをdotfilesにマージ
    gitCopy $dotfilesSecretDir $dotfilesDir HEAD

    # 設定ファイルの配置
    logInfo "Placing dotfiles ..."
    . $supportDir/installDotfiles.sh

    # 利用するソフトウェアのインストール
    logInfo "Installing packages ..."
    . $supportDir/installPackages.sh
    logOk "Successfully Installed packages"

    # ログインシェルの変更を促す
    logInfo "You might want to change login shell by 'chsh --shell `which zsh`'. Or 'echo `which zsh` >> ~/.bashrc'"

    logOk "Installation successfully finished!"
}
run

zsh
