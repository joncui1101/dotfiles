# Source necessary functions
source $DOTDIR/aliases/dir_aliases.sh
source $DOTDIR/aliases/vim_aliases.sh
source $DOTDIR/aliases/git_aliases.sh
if [[ -d $WS/dotfiles ]]; then
    source $WS/dotfiles/work_aliases.sh
fi


if [[ $OSTYPE == *linux* ]]; then
    source $DOTDIR/aliases/fed_aliases.sh
else
    source $DOTDIR/aliases/mac_aliases.sh
fi

# CLI Aliases
alias rm='rm -i '
alias so='source $DOTDIR/zshrc'
alias cl='clear'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Replace ls with exa
if command -v exa &> /dev/null
then
    alias ls='exa'
fi

# Enable aliases to be sudo'ed
alias sudo='sudo '

# Editor Aliases
if command -v nvim &> /dev/null
then
    alias v='nvim '
    alias sv='sudo -E nvim '
fi

# Show history
alias history='fc -il 1'

# Show top 5 cpu intensive processes
alias pcf='ps aux | sort -rk 3,3 | head -n 6'
alias pmf='ps aux | sort -rk 4,4 | head -n 6'

if command -v bat &> /dev/null
then
    alias cat='bat'
fi
