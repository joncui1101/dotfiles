.PHONY: setup

BREW := $(shell command -v brew 2> /dev/null)

setup: Brewfile
ifndef BREW
	$(info "Installing brew")
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
endif

	$(info "Installing packages in Brewfile")
	brew bundle install

