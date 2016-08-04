# OS判定
function ostype() {
    uname| tr "[:upper:]" "[:lower:]"
}
function is_linux() { [[ `ostype` == linux* ]]; }
function is_osx() { [[ `ostype` == darwin* ]]; }
function is_bsd() { [[ `ostype` == bsd* ]]; }
function is_msys() { [[ `ostype` == msys* ]]; }

function is_exists() { type "$1" >/dev/null 2>&1; return $?; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }
