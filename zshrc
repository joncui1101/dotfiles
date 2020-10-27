autoload -U promptinit && promptinit

# You may need to manually set your language environment
export LANG="en_US.UTF-8"
export LANGUAGE=$LANG
export LC_ALL=$LANG
if [[ $OSTYPE == *linux* ]]; then
    export SUDO_EDITOR=rvim
fi

BASE16_SHELL="$HOME/dotfiles/color-schemes/one.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Vi mode
bindkey -v

# source all config files
for config_file ($ZSH/*.zsh*); do
    source $config_file
done
unset config_file

WORKENV=$HOME/work_dot/workenv
if test -f "$WORKENV"; then
    source $WORKENV
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"

kitty + complete setup zsh | source /dev/stdin

if command -v pyenv &> /dev/null
then
    eval "$(pyenv init -)"
fi

eval "$(starship init zsh)"
