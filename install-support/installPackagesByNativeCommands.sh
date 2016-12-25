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
        brew install $package
    else
        die "Cannot detect `ostype`'s package manager"
    fi
}

function runInstallPackages() {
    suggestDependencies
    sourcePath

    mkdir -p $HOME_BIN

    # gcc, binutils
    has gcc || installPackage gcc
    has as || installPackage binutils

    # unzip
    has unzip || installPackage unzip

    # golang (ghqなどのGo制パッケージをインストールするのに必要)
    ## brew install goにしたかったが、Linuxにて下記のように失敗:
    ### Error: SHA256 mismatch
    ### Expected: 141b8345932641483c2437bdbd65488a269282ac85f91170805c273f03dd223b
    ### Actual: ce3140662f45356eb78bc16a88fc7cfb29fb00e18d7c632608245b789b2086d2
    ### Archive: /home/nakatani/.cache/Homebrew/go--gobootstrap-64.tar.gz
    ### To retry an incomplete download, remove the file above.
    if ! has go; then
        local gover='1.6.3'
        local goarch='amd64'

        if is_linux; then
            local goos='linux'
            local gofiletype='tar.gz'
        elif is_osx; then
            local goos='darwin'
            local gofiletype='tar.gz'
        elif is_msys; then
            local goos='windows'
            local gofiletype='zip'
        else
            die "Cannot detect `ostype`'s go archive package"
        fi

        local goarchive=`mktemp`.$gofiletype
        curl https://storage.googleapis.com/golang/go${gover}.${goos}-${goarch}.${gofiletype} -o $goarchive

        extract $goarchive $HOME
        mv $HOME/go $HOME/.go
    fi

    # winpty (minttyでinteractive modeにするためのワークアラウンド)
    is_msys && (has winpty || installPackage winpty)

    # zsh
    has zsh || installPackage zsh

    # zplug
    test -d $HOME/.zplug || (curl -sL zplug.sh/installer |zsh)

    # tmux
    has tmux || installPackage tmux

    # emacs
    has emacs || installPackage emacs-nox
    (
        cd $HOME/.emacs.d
        cask install
    )

    # vim
    has vim || installPackage vim

    # wget
    has wget || installPackage wget

    # inconsolata
    is_ubuntu && (isPackageInstalled fonts-inconsolata || installPackage fonts-inconsolata)

    # ghq
    has ghq || go get github.com/motemen/ghq

    # peco
    has peco || go get github.com/peco/peco/cmd/peco

    # diff-highlight
    if ! has diff-highlight; then
        curl https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight -o $HOME/.bin/diff-highlight
        chmod +x $HOME/.bin/diff-highlight
    fi

    # plantuml.jar
    test -f $HOME/plantuml.jar || curl http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar -o $HOME/plantuml.jar

    # unity-tweak-tool
    is_ubuntu && (has unity-tweak-tool || installPackage unity-tweak-tool)

    # xclip
    is_ubuntu && (has xclip || installPackage xclip)

    # GoldenDict
    is_ubuntu && (has goldendict || installPackage goldendict)

    # scalaenv
    has scalaenv || git clone git://github.com/mazgi/scalaenv.git ~/.scalaenv

    # flyway
    if ! has flyway; then
        if is_linux; then
            (
                cd `mktemp -d`
                curl https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/4.0.3/flyway-commandline-4.0.3-linux-x64.tar.gz -o flyway-commandline-4.0.3-linux-x64.tar.gz
                tar xf flyway-commandline-4.0.3-linux-x64.tar.gz
                mv flyway-4.0.3 $HOME/.flyway
            )
        fi
    fi

    # rbenv
    if ! has rbenv; then
        git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
        git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
    fi

    # nodebrew
    has nodebrew || (curl -L git.io/nodebrew |perl - setup)

    # see: http://totutotu.hatenablog.com/entry/2015/07/26/tmux%E3%81%A7OSX%E3%81%AEopen%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E4%BD%BF%E3%81%88%E3%81%AA%E3%81%84
    if is_osx; then
        has reattach-to-user-namespace || installPackage reattach-to-user-namespace
    fi

    # gsed
    if is_osx; then
        has gsed || installPackage gnu-sed
    fi
}
runInstallPackages