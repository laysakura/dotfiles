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

    # ローカルに存在していなかったら、privateリポジトリから dotfiles-secret を一時的にclone
    # TODO ローカルにあったらそれを使う
    git clone $(repoUrl ${REPO_DOTFILES_SECRET[site]} ${REPO_DOTFILES_SECRET[name]}) "$tmpdir/${REPO_DOTFILES_SECRET[name]}"

    # ローカルに存在していなかったら、publicリポジトリから dotfilesを 一時的にclone
    # TODO ローカルにあったらそれを使う
    git clone $(repoUrl ${REPO_DOTFILES[site]} ${REPO_DOTFILES[name]}) "$tmpdir/${REPO_DOTFILES[name]}"

    # 補助スクリプトが手に入ったのでsource
    local support_dir="$tmpdir/${REPO_DOTFILES[name]}/install-support"
    . "$support_dir/function.sh"

    ostype

    # 各ソフトの設定をばらまく

    # 最低限のシェル関数を定義し、zshやほかのソフトをインストール
}
run




# ログインシェルをzshに変更する
# => zshrcが読み込まれるが、その最初の最初で必要ソフトのインストールを走らせる
# => その次くらいに、現行のinstall.shでやっているようなほかのソフトの設定をやってしまう
