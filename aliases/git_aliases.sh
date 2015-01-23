# Git aliases

alias gm='git merge origin/master --no-ff --no-commit'
alias go='git checkout '
alias gcf='git checkout -- '
alias gs='git status -sb'
alias ga='git add '
alias gb='git branch '
alias gbr='git branch -r '
alias gc='git commit '
alias gca='git commit -a '
alias gdn='git diff -w --numstat '
alias gf='git fetch --all -p '
alias gp='git push origin '
alias gu='git pull origin '
alias gt='git tag '
alias grt='git ls-remote --tags'
alias gg='git grep -p -n --full-name '
alias gl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue) <%an>%Creset' --abbrev-commit"
alias guc='git reset --soft HEAD^'
alias gst='git stash '
alias gsa='git stash apply '
alias gbt=track-branch
alias gcb=create-branch
alias gd=diff-a-b
