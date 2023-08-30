[ -z "$TMUX"  ] && [ -n "$DISPLAY" ] && { exec tmux new-session && exit;}

ZSH_THEME="robbyrussell"

plugins=(
    archlinux
    colored-man-pages
    kubectl
    cp
    git
    fzf
    lein
    autojump
    extract
    copyfile
    rsync
)

# ###########################################################################
# Setup ENV

export DOTFILES=$HOME/dotfiles
export SCRIPTS=$HOME/dotfiles/shell
export SCRIPTS_WORK=$HOME/work
export PYTHON=/usr/local/bin/python3

export ZSH=$HOME/.oh-my-zsh
export EDITOR='nvim'
export VISUAL='nvim'
export EXA_COLORS=$(cat $HOME/.exa-colors)

export KUBECONFIG=$HOME/.kube/config

export JAVA_HOME=/usr/lib/jvm/default

export ERL_AFLAGS="+pc unicode -kernel shell_history enabled -enable-feature all"

# ############################################################################
# Assemble Path

export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:/home/lsund/.yarn/bin"
export PATH="$PATH:/usr/local/lib/node/bin"
export PATH="$PATH:/snap/bin"
export PATH="$PATH:$HOME/.skim/bin"
export PATH="$PATH:$HOME/.bin:/usr/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/usr/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/.cache/rebar3/bin"
export PATH="$PATH:$SCRIPTS"
export PATH="$PATH:$SCRIPTS_WORK"

export FZF_BASE=$DOTFILES/vim/plugged/fzf
export FZF_DEFAULT_COMMAND='fdfind --type f'

[[ -e $DOTFILES/vim/plugged/fzf/shell/key-bindings.zsh ]] &&  . $DOTFILES/vim/plugged/fzf/shell/key-bindings.zsh
[[ -e $DOTFILES/vim/plugged/fzf/shell/completion.zsh ]] &&  . $DOTFILES/vim/plugged/fzf/shell/completion.zsh

export SSH_KEY_PATH="~/.ssh/rsa_id"

source $ZSH/oh-my-zsh.sh

[[ -e /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
[[ -e /usr/share/fzf/completion.zsh ]]   && . /usr/share/fzf/completion.zsh

# ############################################################################
# Setup Autojump

[[ -s /home/lsund/.autojump/etc/profile.d/autojump.sh ]] && source /home/lsund/.autojump/etc/profile.d/autojump.sh

autoload -U compinit && compinit -u

# ############################################################################
# Setup SSH agent

source $HOME/.ssh-agent.bash

# ############################################################################
# Setup sdkman

export SDKMAN_DIR="/Users/lsund/.sdkman"
[[ -s "/Users/lsund/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/lsund/.sdkman/bin/sdkman-init.sh"

[[ -s "/home/lsund/work/aliases.sh" ]] && source "/home/lsund/work/aliases.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ############################################################################
# Source Aliases and widgets

. $HOME/.aliases
. $HOME/.zsh_widgets

bindkey '^o^o' fuzzy-git-branch
bindkey '^z^z' fuzzy-file

fortune | cowsay

[ -f "/Users/lsund/.ghcup/env" ] && source "/Users/lsund/.ghcup/env" # ghcup-env

## Activate Kerl
. /usr/local/lib/erlang/25.3/activate

if [[ $HOST != "renewise" ]] ; then

  # Load Angular CLI autocompletion.
  source <(ng completion script)

  # Load kerl installation
  # Load ASDF
  source ~/.asdf/asdf.sh

  # >>> conda initialize >>>
  # !! Contents within this block are managed by 'conda init' !!
  __conda_setup="$('/home/lsund/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
  if [ $? -eq 0 ]; then
      eval "$__conda_setup"
  else
      if [ -f "/home/lsund/mambaforge/etc/profile.d/conda.sh" ]; then
          . "/home/lsund/mambaforge/etc/profile.d/conda.sh"
      else
          export PATH="/home/lsund/mambaforge/bin:$PATH"
      fi
  fi
  unset __conda_setup

  if [ -f "/home/lsund/mambaforge/etc/profile.d/mamba.sh" ]; then
      . "/home/lsund/mambaforge/etc/profile.d/mamba.sh"
  fi
  # <<< conda initialize <<<

fi
