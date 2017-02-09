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
    else
        die "Not implemented yet"
    fi
}

function runInstallPackages() {
    suggestDependencies
    sourcePath

    mkdir -p $HOME_BIN

    # 色んなパッケージマネージャのインストールに必要なパッケージ
    installPackage gcc
    installPackage binutils
    installPackage autoconf
    is_osx || installPackage unzip

    # 色んなパッケージマネージャのインストール
    installPackage go  # go get
    installPackage cask
    has ghq || go get github.com/motemen/ghq
    has rustc || (curl https://sh.rustup.rs -sSf | sh)  # rustに付属するcargo

    # root package managerでインストールする諸々
    installPackage ant
    installPackage awscli
    installPackage bash
    installPackage emacs
    installPackage flyway
    installPackage git
    installPackage gnu-sed
    installPackage jq
    installPackage libevent
    installPackage libssh2
    installPackage mysql
    installPackage openssl
    installPackage peco
    installPackage pkg-config
    installPackage pyenv
    installPackage readline
    installPackage reattach-to-user-namespace
    installPackage sbt
    installPackage tmux
    installPackage tree
    installPackage vim
    installPackage watch
    installPackage wget
    installPackage zsh

    # caskでインストール
    (
        cd $HOME/.emacs.d
        cask install
    )

    # go getでインストール
    has peco || go get github.com/peco/peco/cmd/peco

    # cargoでインストール
    has racer || cargo install racer
    has rustfmt || cargo install rustfmt

    # ghqでインストール
    ghq get rust-lang/rust

    # その他のコマンドでインストール
    test -d $HOME/.zplug || (curl -sL zplug.sh/installer |zsh)

    if ! has diff-highlight; then
        curl https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight -o $HOME/.bin/diff-highlight
        chmod +x $HOME/.bin/diff-highlight
    fi

    has scalaenv || git clone git://github.com/mazgi/scalaenv.git ~/.scalaenv

    if ! has rbenv; then
        git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
        git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
    fi

    has nodebrew || (curl -L git.io/nodebrew |perl - setup)

    # osx専用パッケージのインストール
    if is_osx; then
        installPackage reattach-to-user-namespace  # see: http://totutotu.hatenablog.com/entry/2015/07/26/tmux%E3%81%A7OSX%E3%81%AEopen%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E4%BD%BF%E3%81%88%E3%81%AA%E3%81%84
        installPackage gnu-sed

        installHomebrewCaskPackage kobito
        installHomebrewCaskPackage meld

        brew tap homebrew/services
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
