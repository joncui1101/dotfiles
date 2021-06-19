# Update homebrew
alias upbrew='brew update; brew upgrade; brew cleanup;'

# Recursively delete `.DS_STORE` files
alias cleanup='find . -type f -name "*.DS_Store" -ls -delete'

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple's System Logs to improve shell startup speed
alias emptytrash='sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl'

# Show/hide hidden files in Finder
alias showF='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hideF='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'

# Alias to open in finder
alias o='open '
alias oo='open .'

# [O]pen [F]ile
of() {
    local fi=`fd --type f --exact-depth 1 . ~/Documents ~/Desktop ~/Downloads | cut -d"/" -f4,5 | fzf --height=40% --header='[open:file]'`

    if [[ $fi ]]; then
        open ~/$fi
    fi
}
