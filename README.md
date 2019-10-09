# Dotfiles

My dotfiles used on Arch Linux and Ubuntu, mainly configuration for ...

- Vim
- Emacs (evil mode)
- Xmonad

... And more than a few *sh utility scripts.

To automatically install symlinks, call `shell/install-symlinks`. This will create the dotfile `$HOME/.$file -> $DOTFILES/$file` link for every configuration file in this repository (ignoring the scripts).
