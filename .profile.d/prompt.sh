_ps1_prefix()
{
    declare -a prefix
    if (( ${_INTERACTIVE_BASH_DEPTH:-0} > 1 )); then
        prefix+=("bash+${_INTERACTIVE_BASH_DEPTH}")
    fi

    if [[ ${prefix} ]]; then
        (IFS=\;; printf '(%s) ' "${prefix[*]}")
    fi
}

_ps1_suffix()
{
    declare -a suffix
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
        local repo_root=$(basename $(git rev-parse --show-toplevel))
        suffix+=("${repo_root}")
        local branch_desc=$(git rev-parse --abbrev-ref HEAD)
        if [[ ${branch_desc} != HEAD ]]; then
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

_reset_ps1()
{
    local prefix=$(_ps1_prefix)
    local suffix=$(_ps1_suffix)
    if [[ -n $TERM && $TERM != dumb ]]; then
        local default_prompt='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]'
    else
        local default_prompt='\u@\h:\W'
    fi
    PS1="${prefix}${default_prompt}${suffix}$ "
}

PROMPT_COMMAND=_reset_ps1
if [[ -n $PS1 ]]; then
    _reset_ps1
fi
