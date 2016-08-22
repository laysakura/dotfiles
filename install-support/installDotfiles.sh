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

# xkb
cpDeep $basedir/.xkb $HOME/
mkdir -p $HOME/.config/autostart
cat > $HOME/.config/autostart/xkb.desktop <<EOS
[Desktop Entry]
Name=xkb
Exec=xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/mykbd $DISPLAY
Type=Application
X-GNOME-Autostart-enabled=true
EOS

# IntelliJ
logWarn 'You may have to rename .IntelliJIdea2016.2/ directory corresponding to IntelliJ version'
cpDeep $basedir/.IntelliJIdea2016.2 $HOME/

# GoldenDict
cpDeep $basedir/.goldendict $HOME/
