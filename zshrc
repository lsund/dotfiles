[ -z "$TMUX"  ] && [ -n "$DISPLAY" ] && { exec tmux new-session && exit;}

ZSH_THEME="robbyrussell"

plugins=(
    archlinux
    colored-man-pages
    cp
    git
    fzf
    lein
    autojump
    extract
    copyfile
    rsync
    taskwarrior
)

[[ -e /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
[[ -e /usr/share/fzf/completion.zsh ]]   && . /usr/share/fzf/completion.zsh

[[ -e $HOME/.fzf/shell/key-bindings.zsh ]] &&  . $HOME/.fzf/shell/key-bindings.zsh
[[ -e $HOME/.fzf/shell/completion.zsh ]] &&  . $HOME/.fzf/shell/completion.zsh


[[ -s /home/lsund/.autojump/etc/profile.d/autojump.sh ]] && source /home/lsund/.autojump/etc/profile.d/autojump.sh

autoload -U compinit && compinit -u

# ###########################################################################
# ENV

export ZSH=$HOME/.oh-my-zsh
export EDITOR='vim'
export VISUAL='vim'
export EXA_COLORS=$(cat $HOME/.exa-colors)
export JAVA_HOME=/usr/lib/jvm/default

export PATH="$PATH:/home/lsund/.yarn/bin"
export PATH="$PATH:/usr/local/lib/node/bin"
export PATH="$PATH:$HOME/.perl-6-install/rakudo-star-2019.03"
export PATH="$PATH:$HOME/.skim/bin"
export PATH="$PATH:$HOME/Documents/work/metro/git/mrc-scripts/lsund/zsh"
export PATH=$PATH:$HOME/.bin:/usr//bin
export PATH=$PATH:$HOME/Documents/dotfiles/shell
export PATH=$PATH:$HOME/.local/bin
export PATH="$PATH:$HOME/.local/usr/bin"
export PATH=$PATH:$HOME/.cabal/bin
export PATH=$PATH:$ANDROID_SDK_HOME/emulator:$ANDROID_HOME/tools

export ANDROID_HOME="/opt/android-sdk"
export ANDROID_SKD_ROOT="/opt/android-sdk"
export ANDROID_SDK_HOME="/opt/android-sdk"
export ANDROID_AVD_HOME="/home/lsund/.android/avd"

export XML_CATALOG_FILES="${HOME}/Data/xml/catalog /etc/xml/catalog"

export DOTFILES=$HOME/Documents/dotfiles
export SCRIPTS=$HOME/Documents/dotfiles/shell

export FZF_DEFAULT_COMMAND='fd --type f'

export SSH_KEY_PATH="~/.ssh/rsa_id"
source $ZSH/oh-my-zsh.sh

# Start ssh-agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi

# Check if identity not already added. In that case, add the identity
ssh-add -l > /dev/null
if [[ $? != "0" ]]; then
    if [[ -e ~/.ssh/id_rsa ]]; then
        echo "Adding id_rsa..."
        ssh-add ~/.ssh/id_rsa
    fi
    if [[ -e ~/.ssh/id_rsa_gmail ]]; then
        echo "Adding id_rsa_gmail..."
        ssh-add ~/.ssh/id_rsa_gmail
    fi
    if [[ -e ~/.ssh/id_rsa_innoq ]]; then
        echo "Adding id_rsa_innoq..."
        ssh-add ~/.ssh/id_rsa_innoq
    fi
fi


# ssh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

. $HOME/.aliases
. $HOME/.zsh_widgets

bindkey '^o^o' fuzzy-git-branch
bindkey '^z^z' fuzzy-file
