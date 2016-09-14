#!/usr/bin/env bash
set -xeu

# 設定
declare -a DEPENDENCIES=(
    'git'
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

# Unity上でのdbusの問題を解決するおまじない -> http://askubuntu.com/questions/457016/how-to-change-gsettings-via-remote-shell
function dbusUnityMagic() {
    # Search these processes for the session variable 
    # (they are run as the current user and have the DBUS session variable set)
    compatiblePrograms=( nautilus kdeinit kded4 pulseaudio trackerd )

    # Attempt to get a program pid
    for index in ${compatiblePrograms[@]}; do
        PID=$(pidof -s ${index})
        if [[ "${PID}" != "" ]]; then
            break
        fi
    done
    if [[ "${PID}" == "" ]]; then
        echo "Could not detect active login session"
        return 1
    fi

    QUERY_ENVIRON="$(tr '\0' '\n' < /proc/${PID}/environ | grep "DBUS_SESSION_BUS_ADDRESS" | cut -d "=" -f 2-)"
    if [[ "${QUERY_ENVIRON}" != "" ]]; then
        export DBUS_SESSION_BUS_ADDRESS="${QUERY_ENVIRON}"
        echo "Connected to session:"
        echo "DBUS_SESSION_BUS_ADDRESS=${DBUS_SESSION_BUS_ADDRESS}"
    else
        echo "Could not find dbus session ID in user environment."
        return 1
    fi

    return 0
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

    # 一回だけ実行すれば良いもの(冪等性は必要)
    is_ubuntu && dbusUnityMagic
    is_ubuntu && gsettings set org.gnome.settings-daemon.plugins.keyboard active false  # fcitx切り替えの際にxkbキーバインドが戻らないようにする
    is_ubuntu && gsettings set org.gnome.desktop.interface gtk-key-theme Emacs          # 操作を全体的にEmacs風に

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
