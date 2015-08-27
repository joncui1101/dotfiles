export PLATFORM=`uname`

if [[ $PLATFORM == 'Darwin' && ! -z $(grunt 2> /dev/null) ]]; then
    eval "$(grunt --completion=bash)"
fi

if [[ $PLATFORM == 'Linux' ]]; then
    if ! pgrep ssh-agent > /dev/null; then
        ssh-agent > ~/.ssh-agent-thing
    fi
    if [[ "$SSH_AGENT_PID" == "" ]]; then
        eval $(<~/.ssh-agent-thing)
    fi
    ssh-add -l >/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'

    eval $(keychain --eval -Q --quiet id_rsa)
fi

[ -n "$PS1" ] && source $HOME/dotfiles/bash_profile
