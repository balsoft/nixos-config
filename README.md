# balsoft’s nixos configuration
## Trying it out in VM
If you happen to run NixOS, you can quite easily check out my configuration by running

```
git clone https://github.com/balsoft/nixos-config --recursive && cd nixos-config && cat vm && read  && ./vm
```

I recommend you inspect `vm` script before running it. Press Ctrl-C if something seems wrong (after all, Microsoft owns Github now; you never know if it’s Windows 10 installation script)

## Creating your own secret.nix for better experience
You can check out the structure of `./secret.nix` in `./modules/secrets.nix`

## Installing it on your machine
Run `./install`
