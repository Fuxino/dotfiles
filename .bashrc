#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Complete commands when using sudo and man
complete -cf sudo
complete -cf doas
complete -c man

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

if [ ! -d ~/.cache/mutt ]; then
   mkdir ~/.cache/mutt
fi

export EDITOR=vim
export SUDO_EDITOR=rvim

export PATH="/home/fuxino/.local/bin:$PATH"

eval "$(starship init bash)"

exec fish
