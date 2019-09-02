.PHONY: install
install: result; SHELL=/bin/sh pkexec $PWD/switch .

secret.nix: secret.nix.gpg; gpg -dq $< > $@

result: secret.nix; ./build

