export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$XDG_CONFIG_HOME/local/share
export XDG_CACHE_HOME=$XDG_CONFIG_HOME/cache
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

export WS=$HOME/workspace/work
export PWS=$HOME/workspace/personal

export CARGO_HOME=$XDG_CONFIG_HOME/cargo
export RUSTUP_HOME=$XDG_CONFIG_HOME/rustup

export GOPATH=$XDG_CONFIG_HOME/go
export GOBIN=$GOPATH/bin

export NVM_DIR=$XDG_CONFIG_HOME/nvm
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/.npmrc
export NPM_CONFIG_CACHE=$XDG_CONFIG_HOME/npm

export PYENV_ROOT=$XDG_CONFIG_HOME/pyenv

export DOTDIR=$PWS/dotfiles
export PATH=/usr/local/opt/mysql@5.7/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/usr/local/bin:$CARGO_HOME/bin:$GOBIN:$DOTDIR/bin:$PYENV_ROOT/bin

export LEDGER=$PWS/ledgers
export LEDGER_FILE=$LEDGER/index.journal

export STARSHIP_CONFIG=$XDG_CONFIG_HOME/starship/starship.toml
export STARSHIP_CACHE=$XDG_CACHE_HOME/starship

export FZF_DEFAULT_COMMAND='rg --files'
export FZF_DEFAULT_OPTS='--layout=reverse --info=inline'
export FZF_COMPLETION_TRIGGER=',,'

export LESSHISTFILE='-'

export MANPAGER='nvim +Man!'
export MANWIDTH=999
#export JAVA_HOME=`/usr/libexec/java_home -v 1.8.0_92`

skip_global_compinit=1
