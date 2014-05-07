# vim:ft=sh et sw=2 ts=2:
# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export SHELL=bash

export PS1='\n\[\e[31m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n$ '

alias ls='ls --color=auto --group-directories-first'
export GREP_OPTIONS='--color=auto'

export EDITOR=vim
export PAGER=less

alias la='ls -la'
alias ll='ls -l'
alias lsa='ls -a'
alias view='vim -R'
alias diffu='colordiff -u'

alias p='pacman -S --needed'
alias syu='pacman -Syu'
alias syua='yaourt -Syua'
alias y='yaourt -S --needed'
alias yy='yaourt -S --needed --noconfirm'
alias ql='pacman -Ql'
alias ssc=systemctl

if [ -e /opt/java7 ]; then
  export JAVA_HOME=/opt/java7
  export PATH="$JAVA_HOME/bin:$PATH"
fi

function vime {
  f="$1"
  if [ -z "$f" ]; then
    echo "USAGE: vime <filename> [<script args>]"
  elif [ ! -x "$f" ]; then
    echo "Not executable: $f"
  else
    shift
    ff="$(dirname "$f")/$(basename "$f")"
    vim "$f" && clear && echo "> $ff $*" && "$ff" "$@"
  fi
}

function mc {
  if [ -z "$1" ]; then
    echo "Directory name required."
  elif [ -e "$1" ]; then
    echo "Dir exists, skipping creation."
    cd "$1"
  else
    mkdir -p "$1" && echo "Dir created."
    cd "$1"
  fi
}
