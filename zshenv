export DOTDIR=$HOME"/dotfiles"
export PATH="/usr/local/sbin:/usr/local/bin:$PATH"

if [ ! -n "$ZSH" ]; then
    ZSH=$DOTDIR/zsh
fi

skip_global_compinit=1

if [[ $OSTYPE == *darwin* ]]; then
    export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/.cask/bin:/Users/jcui/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$DOTDIR/bin:/$HOME/tools/arcanist/bin
else
    export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$DOTDIR/bin
fi
