if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [[ ! -z $(grunt 2> /dev/null) ]]; then
    eval "$(grunt --completion=bash)"
fi

export DOTDIR="/Users/Jonathan/dotfiles"

[ -n "$PS1" ] && source $DOTDIR/bash/bash_profile
