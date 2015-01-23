# activate completion
autoload -U compinit && compinit
if [[ -z $(zmodload | grep complist) ]]; then
    zmodload zsh/complist
fi

setopt auto_menu # get menu behavior when you hit tab again on ambiguous completion
setopt complete_in_word # completion takes place at the cursor position in the word
setopt always_to_end # cursor moves to end of word after it is completed
setopt completealiases # complete aliases

# activate menu selection
zstyle ':completion:*:*:*:*:*' menu select

# activate approximate completion, but only after regular completion (_complete)
zstyle ':completion:::::' completer _complete _approximate
