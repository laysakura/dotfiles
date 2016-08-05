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

# dotfilesリポジトリをcloneするまでに使用する最低限の関数定義
function repoUrl() {
    local repoSite=$1
    local repoName=$2
    echo -n "https://${SITE_USER[$repoSite]}@${SITE_URL[$repoSite]}/$repoName.git"
}
function logError() {
    printf "\033[31m%s\033[m\n" "[ERROR] $*" 1>&2
}
function logInfo() {
    printf "\033[36m%s\033[m\n" "[INFO]  $*" 1>&2
}

# インストール実行
function run() {
    local tmpdir=`mktemp -d`
    local dotfilesDir=$tmpdir/${REPO_DOTFILES[name]}
    local dotfilesSecretDir=$tmpdir/${REPO_DOTFILES_SECRET[name]}

    # ローカルに存在していなかったら、publicリポジトリから dotfilesを 一時的にclone
    # TODO ローカルにあったらそれを使う
    logInfo "Cloning ${REPO_DOTFILES[name]} ..."
    # git clone $(repoUrl ${REPO_DOTFILES[site]} ${REPO_DOTFILES[name]}) $dotfilesDir
    mkdir -p $dotfilesDir ; rmdir $dotfilesDir ; cp -r ../dotfiles $dotfilesDir # TODO 消す

    # ローカルに存在していなかったら、privateリポジトリから dotfiles-secret を一時的にclone
    # TODO ローカルにあったらそれを使う
    logInfo "Cloning ${REPO_DOTFILES_SECRET[name]} ..."
    # git clone $(repoUrl ${REPO_DOTFILES_SECRET[site]} ${REPO_DOTFILES_SECRET[name]}) $dotfilesSecretDir
    mkdir -p $dotfilesSecretDir ; rmdir $dotfilesSecretDir ; cp -r ../dotfiles-secret $dotfilesSecretDir # TODO 消す

    # 補助関数が手に入ったのでsource
    local supportDir=$tmpdir/${REPO_DOTFILES[name]}/install-support
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

    # ログインシェルの変更

    logOk "Installation successfully finished!"
}
run
