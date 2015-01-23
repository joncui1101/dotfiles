autoload colors; colors;
export LSCOLORS="Gxfxcxdxbxegedabagacad"
if [[ "$DISABLE_LS_COLORS" != "true" ]]; then
    version=$(uname -s)
    if [[ "$version" == "NetBSD" ]]; then
        gls --color -d . &>/dev/null 2>&1 && alias ls='gls --color=tty'
    elif [[ "$version" == "OpenBSD" ]]; then
        colorls -G -d . &>/dev/null 2>&1 && alias ls='colorls -G'
    else
        ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
    fi
fi

setopt no_beep
setopt auto_cd
setopt cdablevars
setopt prompt_subst
