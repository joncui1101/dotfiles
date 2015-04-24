autoload -U promptinit && promptinit

# You may need to manually set your language environment
export LANG="en_US.UTF-8"
export LANGUAGE=$LANG
export LC_ALL=$LANG
export TERM=xterm-256color
BASE16_SHELL="$HOME/dotfiles/color-schemes/shell/base16-flat.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# history
setopt APPEND_HISTORY
# for sharing history between zsh processes
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
# ignore duplicate history
setopt HIST_IGNORE_DUPS

# source all config files
for config_file ($ZSH/*.zsh*); do
    source $config_file
done
unset $config_file
