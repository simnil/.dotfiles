if [[ -n $PS1 ]]; then
    export _INTERACTIVE_BASH_DEPTH=$((1+${_INTERACTIVE_BASH_DEPTH:-0}))
fi
