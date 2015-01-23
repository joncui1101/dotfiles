if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [[ ! -z $(grunt 2> /dev/null) ]]; then
    eval "$(grunt --completion=bash)"
fi

[ -n "$PS1" ] && source $HOME/dotfiles/bash_profile
