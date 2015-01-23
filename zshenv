export DOTDIR="/Users/Jonathan/dotfiles"

if [ ! -n "$ZSH" ]; then
    ZSH=$DOTDIR/zsh
fi

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/.cask/bin:/Users/jcui/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$DOTDIR/scripts
