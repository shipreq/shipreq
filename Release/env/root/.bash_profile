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

alias p='pacman -S --needed'
alias syu='pacman -Syu'
alias ql='pacman -Ql'

alias syua='yaourt -Syua'
alias y='yaourt -S --needed'
alias yy='yaourt -S --needed --noconfirm'
