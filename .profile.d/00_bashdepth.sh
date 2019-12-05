if [[ -n $PS1 ]]; then
    export _INTERACTIVE_BASH_DEPTH=$((1+${_INTERACTIVE_BASH_DEPTH:-0}))
    if [[ $TERM =~ screen ]]; then  # both screen and tmux sets TERM=screen
        if [[ -z $_ALREADY_IN_TMUX ]]; then
            _INTERACTIVE_BASH_DEPTH=1
        fi
        export _ALREADY_IN_TMUX=true
    fi
fi
