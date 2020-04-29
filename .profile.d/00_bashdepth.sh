if [[ -n $PS1 ]]; then
    export _INTERACTIVE_BASH_DEPTH=$((1+${_INTERACTIVE_BASH_DEPTH:-0}))
fi

toggle_bash_depth()
{
    if [[ -z $_HIDE_INTERACTIVE_BASH_DEPTH ]]; then
        export _HIDE_INTERACTIVE_BASH_DEPTH=true
    else
        unset _HIDE_INTERACTIVE_BASH_DEPTH
    fi
}
