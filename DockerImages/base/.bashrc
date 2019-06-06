export PS1='\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
export BASH=/bin/bash
export SHELL=$BASH

export LS_OPTIONS='--color=auto'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias la='ls $LS_OPTIONS -la'
