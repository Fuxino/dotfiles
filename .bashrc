#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Complete commands when using sudo and man
complete -cf sudo
complete -c man

# Aliases
alias cinnamon-restart='nohup cinnamon --replace > /dev/null 2>&1 &'
alias clean-aurutils-cache='find ~/.cache/aurutils/sync -type d -name .git -execdir git clean -xi \;'
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias ls='ls -v --color=auto'
alias mutt='pushd ~/Downloads > /dev/null; mutt; popd > /dev/null'
alias ncmpcpp='systemctl --user start mpd-mpris; ncmpcpp; systemctl --user stop mpd-mpris'
alias rm='rm -I'
alias sort='sort -n'
alias sudo='sudo '
alias vi='vim'

if [ "$TERM" == "xterm-kitty" ]; then
    alias ssh='kitten ssh'
fi

# PS1
PS1='[\u@\h \W]\$ '

if [ ! -d ~/.cache/mutt ]; then
   mkdir ~/.cache/mutt
fi

eval "$(beet completion)"

export EDITOR=vim
export SUDO_EDITOR=rvim

if [ "$TERM" == "xterm-kitty" ]; then
    fortune /usr/share/fortune/startrek /usr/share/fortune/anarchism /usr/share/fortune/asoiaf
fi
