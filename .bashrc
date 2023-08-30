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
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias ls='ls -v --color=auto'
alias mutt='pushd ~/Downloads > /dev/null; mutt; popd > /dev/null'
alias set-monitor='xrandr --output HDMI-1-0 --auto --left-of eDP-1 --set "PRIME Synchronization" 1 --output eDP-1 --primary'
alias sort='sort -n'
alias ssh='ssh -X'
alias sudo='sudo '
alias vi='vim'

[ '$TERM'='xterm-kitty' ] && alias ssh='kitty +kitten ssh'

# PS1
PS1='[\u@\h \W]\$ '

if [ ! -d ~/.cache/mutt ]; then
   mkdir ~/.cache/mutt
fi

export SUDO_EDITOR=rvim

export SSH_AGENT_PID=""
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"
