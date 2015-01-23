function getHost {
    prefix=" at %{${CYAN%}%}"
    if [[ ! -z $HOSTNAME ]]; then
        echo "$prefix$HOSTNAME[4]$HOSTNAME[6,12]";
    elif [[ $HOST = "jcui-pro" ]]; then
        echo "${prefix}mbp"
    else
        echo "";
    fi
}

function getUsername {
    if [[ $USERNAME == "jcui" ]]; then
        echo "jon"
    else
        echo $USERNAME
    fi
}

## Function definitions
function preexec {
    case "$2" in
        git*)
        __EXECUTED_GIT_COMMAND=1
        ;;
    esac
}

function precmd {
    if [ -n "$__EXECUTED_GIT_COMMAND" ] || [ -z "$GIT_BRANCH" ]; then
        update_current_git_vars
        unset __EXECUTED_GIT_COMMAND
    fi
}

function chpwd {
    update_current_git_vars
}


function update_current_git_vars() {
    unset GIT_BRANCH
    unset GIT_CHANGED
    unset GIT_STAGED
    unset GIT_CONFLICT
    unset GIT_AHEAD
    unset GIT_BEHIND

    git rev-parse --is-inside-work-tree &>/dev/null || return
    GIT_BRANCH=$(command git symbolic-ref HEAD 2> /dev/null | awk -F'/' '{print $3}')
    GIT_CHANGED=$(command git diff --name-status | grep -v "^U" | wc -l | tr -d ' ')
    GIT_STAGED=$(command git diff --staged --name-status | grep -v "^U" | wc -l | tr -d ' ')
    GIT_CONFLICT=$(command git diff --name-only --diff-filter=U | wc -l | tr -d ' ')
    git_status=$(command git status -sb)
    GIT_AHEAD=$(echo $git_status | grep "ahead" | sed -E "s/.*ahead ([0-9]+).*/\\1/")
    GIT_BEHIND=$(echo $git_status | grep "behind" | sed -E "s/.*behind ([0-9]+).*/\\1/")
}

git_info() {
    precmd
    if [[ -n "$GIT_BRANCH" ]]; then
        STATUS="$GIT_BRANCH"
        SYMBOLS=""
        BRANCH_COLOR="%{${GREEN}%}"
        if [[ "$GIT_STAGED" != "0" ]]; then
            SYMBOLS="$SYMBOLS$ZSH_THEME_GIT_PROMPT_STAGED"
            BRANCH_COLOR="%{${YELLOW}%}"
        fi
        if [[ "$GIT_CHANGED" != "0" ]]; then
            SYMBOLS="$SYMBOLS$ZSH_THEME_GIT_PROMPT_CHANGED"
            BRANCH_COLOR="%{${MAGENTA}%}"
        fi
        if [[ "$GIT_CONFLICT" != "0" ]]; then
            SYMBOLS="$SYMBOLS$ZSH_THEME_GIT_PROMPT_CONFLICT"
            BRANCH_COLOR="%{${RED}%}"
        fi
        STATUS="$BRANCH_COLOR$STATUS%{$reset_color%}$SYMBOLS"
        if [[ -n "$GIT_AHEAD" ]]; then
            STATUS="$STATUS %{${GREEN}%}+$GIT_AHEAD%{$reset_color%}"
        fi
        if [[ -n "$GIT_BEHIND" ]]; then
            STATUS="$STATUS %{${RED}%}-$GIT_BEHIND%{$reset_color%}"
        fi

        echo " on $STATUS%{$reset_color%}"
    fi
}

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_STAGED="%{${YELLOW}%}^%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{${MAGENTA}%}~%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{${RED}%}*%{$reset_color%}"

PROMPT='${BLUE}$(getUsername)%{$reset_color%}$(getHost)%{$reset_color%} in %{${YELLOW%}%}${PWD/#$HOME/~}%{$reset_color%}$(git_info)%{$reset_color%}%b
 $ '

RPROMPT='[%D{%L:%M:%S %p}]'
