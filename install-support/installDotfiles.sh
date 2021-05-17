export basedir=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/..

# ssh
mkdir -p $HOME/.ssh/
cpDeep $basedir/.ssh/config $HOME/.ssh/
cpDeep $basedir/.ssh/id_* $HOME/.ssh/
chmod 600 $HOME/.ssh/*

# zsh
cpDeep $basedir/.zshrc $HOME/
rm -rf $HOME/.zsh ; mkdir $HOME/.zsh
cpDeep $basedir/.zsh/*.zsh $HOME/.zsh/

# emacs
gitCopy $basedir $HOME HEAD:emacs  # el-getのキャッシュは残しつつ、リポジトリの内容をコピー

# tmux
cpDeep $basedir/.tmux.conf $HOME/

# mintty
cpDeep $basedir/.minttyrc $HOME/

# git
cpDeep $basedir/.gitconfig $HOME/
cpDeep $basedir/.gitignore $HOME/

# ghq
is_msys && (cmd /c "mklink /j C:\home C:\msys64\home" || :)  # MSYS2のghqで /home が \home になって、/c/home を参照しに行ってしまう問題のworkaround
mkdir -p $HOME/.ghq

# rust
cpDeep $basedir/.cargo $HOME/

# scala
cpDeep $basedir/.sbt $HOME/
cpDeep $basedir/.sbtrc $HOME/

# python
cpDeep $basedir/.pypirc $HOME/

# gdb
cpDeep $basedir/.gdbinit $HOME/

# .config
cpDeep $basedir/.config $HOME/

# OS X keymap
if is_osx; then
    cp $basedir/MacKeyMap/* "$HOME/Library/Keyboard Layouts/"
fi

# xkb
if is_linux && has_x11 ; then
    cpDeep $basedir/.xkb $HOME/
    cat > $HOME/.config/autostart/xkb.desktop <<EOS
[Desktop Entry]
Name=xkb
Exec=xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/mykbd $DISPLAY
Type=Application
X-GNOME-Autostart-enabled=true
EOS
fi

# 雑多なスクリプト
mkdir -p $HOME/usr/local/bin/
cp -p $basedir/myscripts/* $HOME/.local/bin/
