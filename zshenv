export WS=$HOME/workspace/work
export PWS=$HOME/workspace/personal

ENVFILE=$WS/dotfiles/zsh/zshenv
[ -s $ENVFILE ] && source $ENVFILE

export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$XDG_CONFIG_HOME/local/share
export XDG_CACHE_HOME=$XDG_CONFIG_HOME/cache
export ZDOTDIR=$XDG_CONFIG_HOME/zsh

export CARGO_HOME=$XDG_CONFIG_HOME/cargo
export RUSTUP_HOME=$XDG_CONFIG_HOME/rustup

export GOPATH=$XDG_CONFIG_HOME/go
export GOBIN=$GOPATH/bin

export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/.npmrc
export NPM_CONFIG_CACHE=$XDG_CONFIG_HOME/npm

export DOTDIR=$PWS/dotfiles
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin:/usr/local/bin:$GOBIN:$DOTDIR/bin
if [[ $OSTYPE == darwin* && ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
    export PATH="$PATH:/usr/local/opt/fzf/bin"
fi

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

export ASDF_DATA_DIR=$XDG_CONFIG_HOME/asdf
export ASDF_CONFIG_FILE=$ASDF_DATA_DIR/asdfrc
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME=$ASDF_DATA_DIR/.tool-versions

skip_global_compinit=1
