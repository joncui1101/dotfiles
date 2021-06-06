autoload -U promptinit && promptinit

# You may need to manually set your language environment
export LANG="en_US.UTF-8"
export LANGUAGE=$LANG
export LC_ALL=$LANG
if [[ $OSTYPE == *linux* ]]; then
    export SUDO_EDITOR=rvim
fi

BASE16_SHELL="$DOTDIR/color-schemes/one.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Vi mode
bindkey -v

# source all config files
for config_file ($DOTDIR/zsh/*.zsh*); do
    source $config_file
done
unset config_file

WORKENV=$WS/dotfiles/workenv.zsh
[ -s $WORKENV ] && source $WORKENV

if [[ $OSTYPE == darwin* ]]; then
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
elif [[ $OSTYPE == *linux* ]]; then
    [ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
    [ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
fi

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# kitty + complete setup zsh | source /dev/stdin

if command -v pyenv &> /dev/null
then
    eval "$(pyenv init --path)"
    [ -s $PYENV_ROOT/completions/pyenv.zsh ] && source $PYENV_ROOT/completions/pyenv.zsh
fi


if command -v starship &> /dev/null
then
    eval "$(starship init zsh)"
fi

if [[ `cat $DOTDIR/emacs/env-file` != $PATH ]]; then
    echo "Updating 'env-file'"
    echo $PATH > $DOTDIR/emacs/env-file
fi
