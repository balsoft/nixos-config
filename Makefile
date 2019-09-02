.PHONY: install
install: result; SHELL=/bin/sh pkexec $$PWD/switch $$PWD

secret.nix: secret.nix.gpg; gpg -dq $< > $@

result: secret.nix; ./build

