export DOTDIR=$HOME"/dotfiles"

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$HOME/.cask/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin

# Load the shell dotfiles
[ -r $DOTDIR/bash/bash_prompt ] && source $DOTDIR/bash/bash_prompt
[ -r $DOTDIR/aliases/aliases.sh ] && source $DOTDIR/aliases/aliases.sh

# If possible, add tab completion for many more commands
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# Autocorrect typos in path names when using `cd`
shopt -s cdspell
