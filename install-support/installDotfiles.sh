export basedir=$(cd $(dirname $0); pwd)

# ssh
cp -f $basedir/.ssh/config $HOME/.ssh/
cp -f $basedir/.ssh/id_* $HOME/.ssh/

# ghq
## MSYS2のghqで /home が \home になって、 /c/home を参照しに行ってしまう問題のworkaround
is_msys && cmd /c "mklink /j C:\home C:\msys64\home"



# mkdir -p $HOME/local/bin
# mkdir -p $HOME/local/etc/profile.d
# mkdir -p $HOME/.ssh
# mkdir -p $HOME/.ghq
# mkdir -p $HOME/.matplotlib
# cp -f $basedir/mintty/.minttyrc  $HOME/
# cp -f $basedir/zsh/.zshrc        $HOME/
# cp -rf $basedir/zsh/.zsh         $HOME/
# cp -f $basedir/bash/.bash_profile    $HOME/
# cp -f $basedir/bash/.bashrc*     $HOME/
# cp -f $basedir/git/.gitconfig    $HOME/
# cp -f $basedir/git/diff-highlight    $HOME/local/bin/
# cp -f $basedir/tmux/.tmux.conf   $HOME/
# cp -f $basedir/screen/.screenrc  $HOME/
# cp -f $basedir/ssh/*             $HOME/.ssh/
# cp -f $basedir/gdb/.gdbinit      $HOME/
# chmod 600 $HOME/.ssh/*
# cp -f $basedir/X/.Xmodmap        $HOME/
# cp -f $basedir/ruby/.pryrc       $HOME/
# cp -f $basedir/perl/.pause       $HOME/
# cp -f $basedir/perl/.replyrc     $HOME/
# cp -f $basedir/python/pyflakes-with-pep8     $HOME/local/bin/
# cp -f $basedir/python/pyflakes-with-pep8-py3 $HOME/local/bin/
# cp -f $basedir/python/.pypirc    $HOME/
# cp -f $basedir/python/.theanorc  $HOME/
# cp -f $basedir/node/.npmrc       $HOME/
# cp -f $basedir/less/.lesskey     $HOME/ ; lesskey
# cp -f $basedir/bash-completion/* $HOME/local/etc/profile.d/
# cp -f $basedir/matplotlib/matplotlibrc       $HOME/.matplotlib/
# cp -f $basedir/octave/.octaverc  $HOME/
# cp -f $basedir/jars/*.jar        /usr/local/jars/
# cp -f $basedir/jars/plantuml.jar $HOME/

# # el-getのキャッシュは残しつつ、リポジトリの内容をコピー
# gitCopy $basedir $dotfiles_dir HEAD:emacs
