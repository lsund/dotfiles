# # ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#exports
export LANG=en_US.UTF-8
export LC_MESSAGES="C"
export EDITOR="vim"
export DIALOGRC=~/.dialog/dialogrc

. ~/.aliases

# Prompt
green='\[\e[0;92m\]'
blue='\[\e[0;94m\]'
white='\[\e[0;38m\]'

# autocomplete shell for sudo and back
complete -cf sudo

PS1=$blue'[\u@\h:\w]'$white' $ '

# disable ctrl-S ctl-Q output interuption e survives on in Unix because modern
stty -ixon


source "$HOME/.cargo/env"
