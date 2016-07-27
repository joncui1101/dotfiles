export DOTDIR=$HOME"/dotfiles"

if [[ $OSTYPE == *darwin* ]]; then
    export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/.cask/bin

    # If possible, add tab completion for many more commands
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
fi

export PATH=$HOME/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

# Start startx
[[ $OSTYPE == *linux* && -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# Load the shell dotfiles
[ -r $DOTDIR/bash/bash_prompt ] && source $DOTDIR/bash/bash_prompt
[ -r $DOTDIR/aliases/aliases.sh ] && source $DOTDIR/aliases/aliases.sh

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
