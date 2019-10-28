.PHONY: lint format deploy collect-garbage
.DEFAULT_GOAL = deploy

FILES=$(shell find . -name '*.nix')
BOOT_MOUNTED=$(shell sh -c 'mount | grep /boot >/dev/null; echo $$?')
SECURE_MOUNTED=$(shell sh -c 'mount | grep /secure >/dev/null; echo $$?')

lint:
	@nix-linter $(FILES)

format:
	@nixpkgs-fmt $(FILES)

deploy: lint format
ifeq ($(BOOT_MOUNTED),1)
	$(info /boot is not mounted!)
else ifeq ($(SECURE_MOUNTED),1)
	$(info /secure is not mounted!)
else
	sudo nixos-rebuild switch --upgrade
endif

collect-garbage:
	sudo nix-collect-garbage -d
