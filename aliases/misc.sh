# [K]ill [P]rocess
kp() {
    local pid=`ps -ef | sed 1d | fzf -m --header='[kill:process]' | awk '{print $2}'`

    if [ "x$pid" != "x" ]; then
        echo $pid | xargs kill -${1:-9}
        kp
    fi
}

fp() {
    local loc=`echo $PATH | tr ":" "\n" | fzf --header='[find:path]'`

    if [[ -d $loc ]]; then
        rg --files $loc | rev | cut -d"/" -f1 | rev | eval "fzf --header='[find:exe] => $loc'"
        fp
    fi
}
