autoload -U promptinit && promptinit

# You may need to manually set your language environment
export LANG="en_US.UTF-8"
export LANGUAGE=$LANG
export LC_ALL=$LANG
if [[ $OSTYPE == *linux* ]]; then
    export TERM=rxvt-unicode-256color
    export SUDO_EDITOR=rvim
else
    export TERM=xterm-256color
    export WORKON_HOME=$HOME/.virtualenvs
    export PROJECT_HOME=$HOME/Repos
    source /usr/local/bin/virtualenvwrapper_lazy.sh
    source ~/.profile
fi

BASE16_SHELL="$HOME/dotfiles/color-schemes/shell/base16-flat.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Vi mode
bindkey -v

# source all config files
for config_file ($ZSH/*.zsh*); do
    source $config_file
done
unset config_file

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="/usr/local/opt/node@6/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
