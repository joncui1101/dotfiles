export DOTDIR=$HOME"/dotfiles"
export GOPATH=$HOME/go
export PATH="/usr/local/sbin:/usr/local/bin:$PATH:$GOPATH/bin:/usr/local/opt/go/libexec/bin"

if [ ! -n "$ZSH" ]; then
    ZSH=$DOTDIR/zsh
fi

skip_global_compinit=1

if [[ $OSTYPE == *darwin* ]]; then
    export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/.cask/bin:/Users/jcui/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$DOTDIR/bin:/$HOME/tools/arcanist/bin
else
    export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$DOTDIR/bin
fi
