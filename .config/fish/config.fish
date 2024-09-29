if status is-interactive
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        exec startx -- -keeptty
    end

    source ~/.bash_aliases

    thefuck --alias | source

    fastfetch

    calcurse -d 7

    starship init fish | source
end

