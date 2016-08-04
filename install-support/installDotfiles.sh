export basedir=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/..

# ssh
mkdir -p $HOME/.ssh/
cp -f $basedir/.ssh/config $HOME/.ssh/
cp -f $basedir/.ssh/id_* $HOME/.ssh/
chmod 600 $HOME/.ssh/*

# zsh
cp -f $basedir/.zshrc $HOME/
cp -rf $basedir/.zsh $HOME/

# emacs
gitCopy $basedir $HOME HEAD:emacs  # el-getのキャッシュは残しつつ、リポジトリの内容をコピー

# tmux
cp -f $basedir/.tmux.conf $HOME/

# mintty
cp -f $basedir/.minttyrc $HOME/

# git
cp -f $basedir/.gitconfig $HOME/
cp -f $basedir/.gitignore $HOME/

# ghq
is_msys && cmd /c "mklink /j C:\home C:\msys64\home"  # MSYS2のghqで /home が \home になって、/c/home を参照しに行ってしまう問題のworkaround
mkdir -p $HOME/.ghq

# ruby
cp -f $basedir/.pryrc $HOME/

# python
cp -f $basedir/.pypirc $HOME/
cp -f $basedir/.theanorc $HOME/
cp -r $basedir/.matplotlib $HOME/

# octave
cp -f $basedir/.octaverc $HOME/

# gdb
cp -f $basedir/.gdbinit $HOME/
