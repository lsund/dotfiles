# Dotfiles

My dotfiles used on Arch Linux, mainly configuration for ...

- Vim
- Emacs (evil mode)
- Xmonad
- Zsh

... And more than a few zsh and bash utility scripts.

To automatically install symlinks, call `shell/install-symlinks`. This will create the dotfile `$HOME/.$file -> $DOTFILES/$file` link for every configuration file in this repository (ignoring the scripts).
