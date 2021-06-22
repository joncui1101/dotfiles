# Git aliases
alias g='git '
alias gm='g merge origin/master --no-ff --no-commit'
alias gco='g co '
alias gcb='g co -b '
alias gcf='g co -- '
alias gcd='g co develop'
alias gs='g st -sb'
alias ga='g add '
alias gb='g br '
alias gbr='g br -r '
alias gbd='g br -D '
alias gc='g ci '
alias gca='g ci -a '
alias gdn='g diff -w --numstat '
alias gf='g fe '
alias gfo='g fe origin '
alias gps='g push origin '
alias gpl='g pull origin '
alias gt='g tag '
alias grt='g ls-remote --tags'
alias gg='g grep -p -n --full-name '
alias gl="g log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue) <%an>%Creset' --abbrev-commit"
alias guc='g reset --soft HEAD^'
alias gst='g stash '
alias gsa='g stash apply '
alias gbt='g br -u '
alias gd='g diff -w --color --word-diff'

# [G]it [D]elete [B]ranches
gdb() {
    git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)" | rg -v -e "develop|master" | fzf -m --header='[delete:branches]' | xargs git branch -D
}
