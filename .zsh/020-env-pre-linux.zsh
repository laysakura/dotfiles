is_linux || return 0

localedef -f UTF-8 -i ja_JP ja_JP

export JAVA_HOME=$HOME/.jdk/jdk1.8.0_101

export GOROOT=/usr/local/go
