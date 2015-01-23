function kill-ssh {
    ps aux | grep "ssh " | grep -v "grep" | awk '{print $2}' | xargs kill -9
}

function track-branch {
    count=$#
    if [ $count -eq 2 ]; then
        git branch --set-upstream $1 origin/$2
    else
        if [ $count -eq 1 ]; then
            git branch --set-upstream $1 origin/$1
        else
            echo "Usage: gbt <branch> origin/<branch[1]"
        fi
    fi
}

function create-branch {
    count=$#
    if [ $count -eq 1 ]; then
        git checkout -b $1 origin/master; git push origin $1; track-branch $1;
    else
        if [ $count -eq 2 ]; then
            git checkout -b $1 origin/$2; git push origin $1;
        fi
    fi
}

function diff-a-b {
    count=$#
    branch=$(git symbolic-ref -q HEAD | sed -e 's|^refs/heads/||')
    if [ "$count" -eq 2 ]; then
        if [ "$1" = -- ]; then
            git diff -w --color --word-diff $1 $2
        else
            git diff -w --color --word-diff origin/$1 origin/$2
        fi
    elif [ "$count" -eq 1 ] && [ "$branch" != "$1" ]; then
        git diff -w --color --word-diff origin/$1 origin/$branch
    else
        git diff -w --color --word-diff
    fi
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

