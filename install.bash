#!/bin/bash -xeu

# 設定
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

# インストール実行
function run() {
    local tmpdir=`mktemp -d`
    local dotfilesDir=$tmpdir/${REPO_DOTFILES[name]}
    local dotfilesSecretDir=$tmpdir/${REPO_DOTFILES_SECRET[name]}

    # ローカルに存在していなかったら、publicリポジトリから dotfilesを 一時的にclone
    # TODO ローカルにあったらそれを使う
    git clone $(repoUrl ${REPO_DOTFILES[site]} ${REPO_DOTFILES[name]}) $dotfilesDir

    # ローカルに存在していなかったら、privateリポジトリから dotfiles-secret を一時的にclone
    # TODO ローカルにあったらそれを使う
    git clone $(repoUrl ${REPO_DOTFILES_SECRET[site]} ${REPO_DOTFILES_SECRET[name]}) $dotfilesSecretDir

    # 補助スクリプトが手に入ったのでsourceしていく
    local supportDir=$tmpdir/${REPO_DOTFILES[name]}/install-support
    ## 便利関数定義
    . $supportDir/function.sh

    # dotfiles-secretをdotfilesにマージ
    gitCopy $dotfilesSecretDir $dotfilesDir

    # 設定ファイルの配置
    . $supportDir/installDotfiles.sh

    # 利用するソフトウェアのインストール
    . $supportDir/installPackages.sh

    # ログインシェルの変更
}
run
