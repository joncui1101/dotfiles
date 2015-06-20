# Source necessary functions
source $DOTDIR/functions.sh
source $DOTDIR/aliases/dir_aliases.sh
source $DOTDIR/aliases/vim_aliases.sh
source $DOTDIR/aliases/git_aliases.sh
if [[ -a $HOME/work_aliases.sh ]]; then
    source $HOME/work_aliases.sh
fi

# Update Dotfiles
alias upvim='~/dotfiles/scripts/update'

# CLI Aliases
alias rm='rm -i '
alias o='open '
alias oo='open .'
alias ks=kill-ssh
alias so=source-config
alias cl='clear'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# List all files colorized in long format
alias l='ls -lF -G'

# List all files colorized in long format, including dotfiles
alias la='ls -laF -G'

# List only directories
alias lsd='ls -lF -G | grep "^d"'

# Always use color output for `ls`
alias ls='ls -G '
export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=04;31:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

# Enable aliases to be sudo'ed
alias sudo='sudo '

# Update homebrew
alias upbrew='brew update; brew upgrade; brew cleanup;'

# Recursively delete `.DS_STORE` files
alias cleanup='find . -type f -name "*.DS_Store" -ls -delete'

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple's System Logs to improve shell startup speed
alias emptytrash='sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl'

# Show/hide hidden files in Finder
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'

# Editor Aliases
alias emacs='emacs '
alias v='vim '

# Tmux Aliases
alias tn='tmux new -s jcui'
alias t0='tmux attach -t jcui'

# Show history
alias history='fc -il 1'

# Show top 5 cpu intensive processes
alias pcf='ps aux | sort -rk 3,3 | head -n 6'
alias pmf='ps aux | sort -rk 4,4 | head -n 6'
