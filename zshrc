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
    [ -s "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
elif [[ $OSTYPE == *linux* ]]; then
    [ -s /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
    [ -s /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
    [ -s /opt/asdf-vm/asdf.sh ] && source /opt/asdf-vm/asdf.sh
fi

if command -v kitty &> /dev/null
then
    kitty + complete setup zsh | source /dev/stdin
fi

if command -v pyenv &> /dev/null
then
    eval "$(pyenv init --path)"
    [ -s $PYENV_ROOT/completions/pyenv.zsh ] && source $PYENV_ROOT/completions/pyenv.zsh
fi


if command -v starship &> /dev/null
then
    eval "$(starship init zsh)"
fi

if [[ ! -s $DOTDIR/emacs/env-file || `cat $DOTDIR/emacs/env-file` != $PATH ]]; then
    echo "Updating 'env-file'"
    echo $PATH > $DOTDIR/emacs/env-file
fi
