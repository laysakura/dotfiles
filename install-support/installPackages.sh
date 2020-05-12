export basedir=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/..

function suggestDependencies() {
    for d in ${DEPENDENCIES[@]}; do
        has $d || die "Missing command $d: Following commands are required before dotfiles' installation - `echo ${DEPENDENCIES[@]}`"
    done
}

function sourcePath() {
    # やや乱暴だが、PATH含めてzshの環境変数を全部取ってくる
    for f in $basedir/.zsh/[0-9]*env*.zsh; do . $f; done
}

function installPackage() {
    local package=$1

    if is_linux; then
        if is_ubuntu; then
            logInfo "Installing $package ..."
            sudo apt install $package
        else
            logWarn 'Linux package installation is only supported in Ubuntu'
        fi
    elif is_msys; then
        pacman -S $package
    elif is_osx; then
        installByHomebrewIfNotExists $package
    else
        die "Cannot detect `ostype`'s package manager"
    fi
}

function installHomebrewCaskPackage() {
    local package=$1

    if is_osx; then
        installByHomebrewCaskIfNotExists $package
    else
        die "Cannot detect `ostype`'s package manager"
    fi
}

function installRootPackageManager() {
    if is_osx; then
        # $HOME/.hommebrew にHomebrewをインストール
        has brew || curl -L https://gist.githubusercontent.com/kenchan0130/b2b5fec12a8f5e08bb9a3556e2d8bcca/raw | ruby
    elif is_ubuntu; then
        has apt  # assertion
    else
        die "Not implemented yet"
    fi
}

function runInstallPackages() {
    suggestDependencies
    sourcePath

    mkdir -p $HOME_BIN

    # 何をインストールするにせよ必要なパッケージ
    installPackage gcc
    installPackage binutils
    installPackage autoconf
    is_osx || installPackage unzip

    # 途中でコケたときの復旧とか楽にするため、配置済みの .zsh/ は読んでくれるzshを先んじてインストール
    installPackage zsh

    # 色んなパッケージマネージャのインストール

    ## go get
    if is_osx; then
        installPackage go
    elif is_ubuntu; then
        sudo add-apt-repository ppa:longsleep/golang-backports
        sudo apt update
        sudo apt install golang-go
    fi
    mkdir -p $GOROOT

    is_osx && installPackage cask || :

    has ghq || go get github.com/motemen/ghq

    has rustc || (curl https://sh.rustup.rs -sSf | sh)  # rustに付属するcargo

    # root package managerでインストールする諸々
    installPackage emacs
    installPackage peco
    installPackage pkg-config
    installPackage readline
    installPackage reattach-to-user-namespace
    installPackage tmux
    installPackage tree
    installPackage vim
    installPackage watch
    installPackage wget
    installPackage zplug

    # caskでインストール
    (
        cd $HOME/.emacs.d
        cask install
    )

    # その他のコマンドでインストール
    if ! has diff-highlight; then
        curl https://raw.githubusercontent.com/git/git/3dbfe2b8ae94cbdae5f3d32581aedaa5510fdc87/contrib/diff-highlight/diff-highlight -o $HOME/.bin/diff-highlight
        chmod +x $HOME/.bin/diff-highlight
    fi

    # osx専用パッケージのインストール
    if is_osx; then
        installPackage reattach-to-user-namespace  # see: http://totutotu.hatenablog.com/entry/2015/07/26/tmux%E3%81%A7OSX%E3%81%AEopen%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E4%BD%BF%E3%81%88%E3%81%AA%E3%81%84
        installPackage gnu-sed
        installPackage terminal-notifier

        brew tap homebrew/services

        brew tap AdoptOpenJDK/openjdk
        installHomebrewCaskPackage adoptopenjdk11
    fi

    # ubuntu専用パッケージのインストール
    if is_ubuntu; then
        (isPackageInstalled fonts-inconsolata || installPackage fonts-inconsolata)
        (has unity-tweak-tool || installPackage unity-tweak-tool)
        (has xclip || installPackage xclip)
        (has goldendict || installPackage goldendict)
    fi

    # msys専用パッケージのインストール
    is_msys && (has winpty || installPackage winpty)  # winpty (minttyでinteractive modeにするためのワークアラウンド)

    :
}

installRootPackageManager
runInstallPackages
