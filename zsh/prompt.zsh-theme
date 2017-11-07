## Function definitions
function preexec {
    case "$2" in
        g*)
        __EXECUTED_GIT_COMMAND=1
        ;;
        v*)
        __EXECUTED_VIM=1
        ;;
    esac
}

function precmd {
    if [ -n "$__EXECUTED_GIT_COMMAND" ] || [ -n "$__EXECUTED_VIM" ] || [ -z "$GIT_BRANCH" ]; then
        update_current_git_vars
        unset __EXECUTED_GIT_COMMAND
        unset __EXECUTED_VIM
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
    GIT_BRANCH=$(command git symbolic-ref --short HEAD 2> /dev/null)
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
        SYMBOLS="%{$reset_color%}"
        BRANCH_COLOR="%{$GREEN%}"
        if [[ "$GIT_STAGED" != "0" ]]; then
            SYMBOLS+="%{$YELLOW%}^%{$reset_color%}"
            BRANCH_COLOR="%{$YELLOW%}"
        fi
        if [[ "$GIT_CHANGED" != "0" ]]; then
            SYMBOLS+="%{$MAGENTA%}~%{$reset_color%}"
            BRANCH_COLOR="%{$MAGENTA%}"
        fi
        if [[ "$GIT_CONFLICT" != "0" ]]; then
            SYMBOLS+="%{$RED%}*%{$reset_color%}"
            BRANCH_COLOR="%{$RED%}"
        fi
        STATUS="$BRANCH_COLOR$GIT_BRANCH$SYMBOLS"
        if [[ -n "$GIT_AHEAD" ]]; then
            STATUS+="%{$GREEN%}[+$GIT_AHEAD]%{$reset_color%}"
        fi
        if [[ -n "$GIT_BEHIND" ]]; then
            STATUS+="%{$RED%}[-$GIT_BEHIND]%{$reset_color%}"
        fi

        echo " on $STATUS"
    fi
}

PROMPT='%{$BLUE%}崔%{$reset_color%} in %{$YELLOW%}%{${PWD/#$HOME/~}%}%{$reset_color%}$(git_info)%b
 $ '

RPROMPT='[%D{%L:%M:%S %p}]'
