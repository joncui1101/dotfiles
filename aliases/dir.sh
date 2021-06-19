# Directory Aliases
alias dl='cd ~/Downloads'
alias doc='cd ~/Documents'
alias dt='cd ~/Desktop'
alias dot='cd $DOTDIR'
alias lgr='cd $LEDGER'
alias pws='cd $PWS'

# [S]witch [P]roject
sp() {
    local pj=`fd --exact-depth 2 . ~/workspace | cut -d"/" -f5,6 | fzf --height=40% --header='[switch:project]'`

    if [[ $pj ]]; then
        cd ~/workspace/$pj
    fi
}
