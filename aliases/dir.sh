# Directory Aliases
alias dl='cd ~/Downloads'
alias doc='cd ~/Documents'
alias dt='cd ~/Desktop'
alias dot='cd $DOTDIR'
[ -d $LEDGER ] && alias lgr='cd $LEDGER'
alias pws='cd $PWS'

# [S]witch [P]roject
sp() {
    local pw=$PWS
    if [ -d $WS ]; then
        pw+=" $WS"
    fi

    if [ -n $WORK_DIR ]; then
        pw+=" $WORK_DIR"
    fi
    local pj=`echo $pw | xargs fd --exact-depth 1 . | cut -d"/" -f4,5,6 | fzf --height=40% --header='[switch:project]'`

    if [[ $pj ]]; then
        cd ~/$pj
    fi
}
