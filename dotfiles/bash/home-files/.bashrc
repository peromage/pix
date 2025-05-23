### .bashrc  -- Bash init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Sanity checks
## Interactive mode only
[[ "$-" == *i* ]] || return 1

## Emacs TRAMP mode
[[ "$TERM" =~ [Dd]umb ]] && PS1="$ " && return 2

### Interactive shell initialization
## Global variables
declare -A MYENV
MYENV[root_dir]="$(dirname "$(realpath -s "${BASH_SOURCE[0]}")")" ## where this script is (no follow)
MYENV[custom]="${MYENV[root_dir]}/.bashrc-custom"
MYENV[os_windows]=$([[ "$OS" =~ [Ww]indows ]] && echo 1)

## Functions
function join_str {
    ## Usage: join_str delimiter string1 string2 ...
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}

function set_env {
    for i in "$@"; do "__env_$i"; done
}

## Env functions
function __env_editor-mg {
    export EDITOR="mg"
}

function __env_editor-vim {
    export EDITOR="vim"
}

function __env_fcitx {
    export GTK_IM_MODULE="fcitx"
    export QT_IM_MODULE="fcitx"
    export XMODIFIERS="@im=fcitx"
}

function __env_firefox-wayland {
    export MOZ_ENABLE_WAYLAND=1
}

function __env_firefox-x11 {
    ## Fix touchpad kinetic scrolling
    export MOZ_USE_XINPUT2=1
}

function __env_gpg-agent {
    export "GPG_TTY=$(tty)"
    gpg-connect-agent updatestartuptty /bye >/dev/null;
}

function __env_gpg-agent-ssh {
    unset SSH_AGENT_PID
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
}

function __env_path {
    export PATH="\
${HOME}/bin\
:${HOME}/.local/bin\
:${HOME}/.dotnet/tools\
:$PATH"
}

function __env_prompt-classic {
    case "$(id -u)" in
        0) PS1='\[\e[1;30m\][\[\e[0;1;31m\]\u\[\e[1;30m\]@\[\e[0;1;31m\]\h \[\e[0;1;31m\]\w\[\e[1;30m\]]#\[\e[0m\] ';;
        *) PS1='\[\e[1;30m\][\[\e[0;1;34m\]\u\[\e[1;30m\]@\[\e[0;1;34m\]\h \[\e[0;1;36m\]\w\[\e[1;30m\]]$\[\e[0m\] ';;
    esac
}

function __env_shell-bash {
    export SHELL="/usr/bin/bash"
}

function __env_shell-fish {
    export SHELL="/usr/bin/fish"
}

function __env_shell-pwsh {
    export SHELL="/usr/bin/pwsh"
}

function __env_xdg {
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_STATE_HOME="$HOME/.local/state"
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_CACHE_HOME="$HOME/.cache"
}

### Set it up
## The first argument is a string of flags separated by `:' (supposedly).
[[ "$1" == *noenv* ]] || {
    set_env path
    set_env gpg-agent
    set_env gpg-agent-ssh
    set_env prompt-classic
}

## History
shopt -s histappend
HISTCONTROL=ignoredups
HISTFILESIZE=100000
HISTSIZE=10000
PROMPT_COMMAND="history -a" # Update history immediately

## Random stuff
[[ -e "${MYENV[custom]}" ]] && source "${MYENV[custom]}"
