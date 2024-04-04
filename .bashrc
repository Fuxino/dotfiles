#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Complete commands when using sudo and man
complete -cf sudo
complete -c man

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# PS1
# PS1='[\u@\h \W]\$ '

if [ ! -d ~/.cache/mutt ]; then
   mkdir ~/.cache/mutt
fi

eval "$(beet completion)"

export EDITOR=vim
export SUDO_EDITOR=rvim

fastfetch

eval "$(starship init bash)"
