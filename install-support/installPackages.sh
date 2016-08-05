function suggestDependencies() {
    for d in ${DEPENDENCIES[@]}; do
        has $d || die "Missing command $d: Following commands are required before dotfiles' installation - `echo ${DEPENDENCIES[@]}`"
    done
}

function installNonRootPackageManager() {
    if [ is_linux ]; then
        if ! has brew; then
            ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
            export PATH=$HOME/.linuxbrew/bin:$PATH
        fi
        brew update
    elif [ is_msys ]; then
        has pacman || die "MSYS2 is supposed to have pacman"
    else
        die "Cannot detect `ostype`'s package manager"
    fi
}

function installPackage() {
    local package=$1

    if [ is_linux ]; then
        brew install $package
    elif [ is_msys ]; then
        pacman -S $package
    else
        die "Cannot detect `ostype`'s package manager"
    fi
}

function runInstallPackages() {
    suggestDependencies
    installNonRootPackageManager

    mkdir -p $HOME_BIN

    # gcc (brewでsource packageをインストールするのに必要)
    has gcc || installPackage gcc

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

        export GOROOT=$HOME/.go
        export GOPATH=$HOME/.ghq
        export PATH=$GOROOT/bin:$GOPATH/bin:$PATH
    fi

    # zsh
    has zsh || installPackage zsh

    # tmux
    has tmux || installPackage tmux

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
}
runInstallPackages
