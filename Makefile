.PHONY: install
install: result; SHELL=/bin/sh pkexec ./switch .

secret.nix: secret.nix.gpg; gpg -dq $< > $@

result: secret.nix; ./build

