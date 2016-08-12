function kill-ssh {
    ps aux | grep "ssh " | grep -v "grep" | awk '{print $2}' | xargs kill -9
}

function source-config {
    case $SHELL in
        */zsh)
            echo "Sourcing zshrc"
            source $DOTDIR/zshrc
        ;;
        */bash)
            echo "Sourcing bashrc"
            source $DOTDIR/bashrc
        ;;
    esac
}

