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

WORKENV=$HOME/work_dot/workenv
[ -s $WORKENV ] && source $WORKENV

if [[ $OSTYPE == darwin* ]]; then
    [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh
elif [[ $OSTYPE == *linux* ]]; then
    [ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
    [ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
fi

[ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

kitty + complete setup zsh | source /dev/stdin

if command -v pyenv &> /dev/null
then
    eval "$(pyenv init -)"
    [ -s $PYENV_ROOT/completions/pyenv.zsh ] && source $PYENV_ROOT/completions/pyenv.zsh
fi


if command -v starship &> /dev/null
then
    eval "$(starship init zsh)"
fi
