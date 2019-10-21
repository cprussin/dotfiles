.PHONY: lint format
.DEFAULT_GOAL = deploy

FILES=$(shell find . -name '*.nix')

lint:
	@nix-linter $(FILES)

format:
	@nixpkgs-fmt $(FILES)

deploy: lint format
	sudo nixos-rebuild switch --upgrade
