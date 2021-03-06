_ps1_prefix()
{
    declare -a prefix
    if (( ${_INTERACTIVE_BASH_DEPTH:-0} > 1 )) && [[ -z $_HIDE_INTERACTIVE_BASH_DEPTH ]]; then
        prefix+=("bash+${_INTERACTIVE_BASH_DEPTH}")
    fi

    if [[ -n $VIRTUAL_ENV ]]; then
        prefix+=("$(basename "${VIRTUAL_ENV}")")
    fi

    if [[ -n $TERM && $TERM != dumb ]]; then
        local format_str='\[\033[36m\](%s)\[\033[39m\] '
    else
        local format_str='(%s) '
    fi

    if [[ ${prefix} ]]; then
        (IFS=\;; printf "${format_str}" "${prefix[*]}")
    fi
}

_ps1_suffix()
{
    declare -a suffix
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
        local repo_root=$(basename $(git rev-parse --show-toplevel))
        suffix+=("${repo_root}")

        local branch_desc
        branch_desc=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
        local branch_exit_code=$?

        if [[ ${branch_exit_code} != 0 ]]; then
            suffix+=("< no head >")
        elif [[ ${branch_desc} != HEAD ]]; then
            suffix+=("${branch_desc}")
        else
            local tag_desc=$(git describe --tags 2>/dev/null)
            if [[ -n ${tag_desc} ]]; then
                suffix+=("#${tag_desc}")
            else
                suffix+=("$(git rev-parse --short HEAD)...")
            fi
        fi

        local work_tree_modifications=$(git status --porcelain)
        if [[ -n ${work_tree_modifications} ]]; then
            suffix+=('𝚫') # U+1D6AB
        fi

        if [[ -n $TERM && $TERM != dumb ]]; then
            local format_str=' \[\033[33m\]{ %s }\[\033[39m\] '
        else
            local format_str=' { %s } '
        fi
    fi

    if [[ ${suffix} ]]; then
        (IFS=:; printf "${format_str}" "${suffix[*]}")
    fi
}

_prompt_expanded_length()
{
    local without_unreadables=$(perl -pe 's/\\\[.*?\\\]//g' <<< "$*")
    local expanded=${without_unreadables@P}
    echo ${#expanded}
}

_reset_ps1()
{
    local prefix=$(_ps1_prefix)
    local suffix=$(_ps1_suffix)
    if [[ -n $TERM && $TERM != dumb ]]; then
        local default_prompt='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]'
    else
        local default_prompt='\u@\h:\W'
    fi
    local prompt="${prefix}${default_prompt}${suffix}"

    local prompt_length="$(_prompt_expanded_length "${prompt}") + 3"
    local max_length="0.5 * $(tput cols)"
    local prompt_too_long=$(bc <<< "scale=2; (${prompt_length}) > (${max_length})")
    if [[ ${prompt_too_long} = 1 ]]; then
        local end='\n$ '
    else
        local end='$ '
    fi

    PS1="${prompt}${end}"
}

PROMPT_COMMAND=_reset_ps1
if [[ -n $PS1 ]]; then
    _reset_ps1
fi
