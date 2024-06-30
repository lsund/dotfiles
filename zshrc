[ -z "$TMUX"  ] && [ -n "$DISPLAY" ] && { exec tmux new-session && exit;}

ZSH_THEME="robbyrussell"

plugins=(
    # archlinux
    kubectl
    cp
    git
    fzf
    autojump
    extract
    copyfile
)

# ###########################################################################
# Setup ENV

export DOTFILES=$HOME/dotfiles
export SCRIPTS=$HOME/dotfiles/shell

if [[ $HOST == "N52930" ]]; then
  export PYTHON=/usr/bin/python3
else
  export PYTHON=/usr/local/bin/python3
fi

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

export FZF_BASE=$DOTFILES/vim/plugged/fzf
export FZF_DEFAULT_COMMAND='fdfind --type f'

[[ -e $DOTFILES/vim/plugged/fzf/shell/key-bindings.zsh ]] &&  . $DOTFILES/vim/plugged/fzf/shell/key-bindings.zsh
[[ -e $DOTFILES/vim/plugged/fzf/shell/completion.zsh ]] &&  . $DOTFILES/vim/plugged/fzf/shell/completion.zsh

export SSH_KEY_PATH="~/.ssh/rsa_id"

source $ZSH/oh-my-zsh.sh

[[ -e /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
[[ -e /usr/share/fzf/completion.zsh ]]   && . /usr/share/fzf/completion.zsh

# ############################################################################
# Setup SSH agent

source $HOME/.ssh-agent.bash

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ############################################################################
# Source Aliases and widgets

# . /home/lsund/kerl/25.3/activate

. $HOME/.aliases
. $HOME/.zsh_widgets

fortune | cowsay

