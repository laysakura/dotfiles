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

# ruby
cpDeep $basedir/.pryrc $HOME/

# python
cpDeep $basedir/.pypirc $HOME/
cpDeep $basedir/.theanorc $HOME/
cpDeep $basedir/.matplotlib $HOME/

# octave
cpDeep $basedir/.octaverc $HOME/

# gdb
cpDeep $basedir/.gdbinit $HOME/

# mysql
cpDeep $basedir/.my.cnf $HOME/

# .config
cpDeep $basedir/.config $HOME/

# OS X keymap
if is_osx; then
    cp $basedir/MacKeyMap/* "$HOME/Library/Keyboard Layouts/"
fi

# xkb
if is_linux; then
    cpDeep $basedir/.xkb $HOME/
    cat > $HOME/.config/autostart/xkb.desktop <<EOS
[Desktop Entry]
Name=xkb
Exec=xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/mykbd $DISPLAY
Type=Application
X-GNOME-Autostart-enabled=true
EOS
fi

# IntelliJ
if is_linux; then
    cpDeep $basedir/IdeaIC2017.3 $HOME/
elif is_osx; then
    mkdir -p $HOME/Library/Preferences/IdeaIC2017.3
    cpDeep $basedir/IdeaIC2017.3/idea.vmoptions $HOME/Library/Preferences/IdeaIC2017.3/idea.vmoptions
    cpDeep $basedir/IdeaIC2017.3/settings.jar $HOME/Library/Preferences/IdeaIC2017.3/settings.jar
fi

# RubyMine
if is_linux; then
    cpDeep $basedir/.RubyMine2016.2 $HOME/
elif is_osx; then
    mkdir -p $HOME/Library/Preferences/RubyMine2016.2
    cpDeep $basedir/.RubyMine2016.2/rubymine.vmoptions $HOME/Library/Preferences/RubyMine2016.2/rubymine.vmoptions
fi

# Visual Studio Code
if is_osx; then
    mkdir -p "$HOME/Library/Application Support/Code"
    cpDeep $basedir/vscode/* '"$HOME/Library/Application Support/Code"'  # cpDeepの中で展開されたときにダブルクオートが残るように
fi

# GoldenDict
cpDeep $basedir/.goldendict $HOME/

# 雑多なスクリプト
cp -p $basedir/myscripts/* $HOME/usr/local/bin/
