#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
export PATH=$PATH:~/Scripts/manual
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.cabal/bin

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/lsund/.sdkman"
[[ -s "/Users/lsund/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/lsund/.sdkman/bin/sdkman-init.sh"
