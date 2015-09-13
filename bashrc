if [[ $OSTYPE == *darwin* && ! -z $(grunt 2> /dev/null) ]]; then
    eval "$(grunt --completion=bash)"
fi

if [[ $OSTYPE == *linux* ]]; then
    if [[ "$SSH_AGENT_PID" == "" ]]; then
        eval $(<~/.ssh-agent-thing)
    fi
    ssh-add -l >/dev/null

    eval $(keychain --eval -Q --quiet id_rsa)
fi

[ -n "$PS1" ] && source $HOME/dotfiles/bash_profile
