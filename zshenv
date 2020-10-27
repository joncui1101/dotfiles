export DOTDIR=$HOME/dotfiles
export GOPATH=$HOME/workspace/go
export ZSH=$DOTDIR/zsh
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/usr/local/bin:$HOME/.cargo/bin:$GOPATH/bin:$DOTDIR/bin
export LEDGER=$HOME/ledgers
export LEDGER_FILE=$LEDGER/index.journal

export PYENV_ROOT=$HOME/.config/pyenv

export FZF_DEFAULT_COMMAND='rg --files'
export FZF_DEFAULT_OPTS='--layout=reverse --info=inline'
export FZF_COMPLETION_TRIGGER=',,'

export MANPAGER='nvim +Man!'
export MANWIDTH=999
#export JAVA_HOME=`/usr/libexec/java_home -v 1.8.0_92`

skip_global_compinit=1

if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s $HOME/.zshrc ]]; then
    source $HOME/.zshrc
fi
