# balsoft’s nixos configuration

## License

Most of this config is in public domain (see [LICENSE](./LICENSE)).

## Stuff that may be useful

### Secrets

Secrets are kept in a separate (private) git repository, encrypted with
gpg and decrypted at runtime using [secrets.nix](./modules/secrets.nix)
and [secrets-envsubst.nix](./modules/secrets-envsubst.nix). The repo is
`pass(1)`-compatible, so passwords are also stored there.

_pls no pwn_

### Themes

Themes for everything are generated from a custom base16 theme. The theme
is defined in [themes.nix](./modules/themes.nix), and the generation is spread all around
[modules](./modules).

### Tmpfs root

To prevent extraneous state from clinging on the drive, I am using tmpfs
root on my two main devices. It is implemented in [persist.nix](./modules/persist.nix).

### Easy Wireguard setup module

Copied from notgne2 with permission to redistribute as public domain software.
Can be found in [ezwg.nix](./modules/ezwg.nix)

## Installing it on your machine

1. Add a config for your device to `machines` (it has to set `deviceSpecific.devInfo`, import your `hardware-configuration.nix` and one of the profiles, and contain a `system` file);
2. `sudo nixos-rebuild test --flake .`
