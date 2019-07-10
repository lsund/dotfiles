# ###########################################################################
# Extend path

export PATH="$PATH:$HOME/.skim/bin"
export PATH=$PATH:$HOME/.bin:/usr/local/bin
export PATH=$PATH:$HOME/Documents/dotfiles/shell
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.cabal/bin
export PATH=$PATH:$HOME/Downloads/rakudo-star-2018.10/install/bin
export PATH=$PATH:$HOME/Downloads/rakudo-star-2018.10/install/share/perl6/site/bin
export PATH=$PATH:$ANDROID_SDK_HOME/emulator:$ANDROID_HOME/tools
exa_permission_bit_colors="ur=0:uw=0:ux=0:ue=0:gr=0:gw=0:gx=0:tr=0:tw=0:tx=0"
exa_filesize_colors="sn=37:sb=37"
exa_file_colors="ex=32;1"

export EXA_COLORS="ds=0:uu=0:gu=0:da=37:di=1;34:$exa_filesize_colors:$exa_permission_bit_colors:$exa_file_colors"


# ###########################################################################
# Custom env variables

export ANDROID_HOME="/opt/android-sdk"
export ANDROID_SKD_ROOT="/opt/android-sdk"
export ANDROID_SDK_HOME="/opt/android-sdk"
export ANDROID_AVD_HOME="/home/lsund/.android/avd"

export XML_CATALOG_FILES="${HOME}/Data/xml/catalog /etc/xml/catalog"

export DOTFILES=$HOME/Documents/dotfiles
export SCRIPTS=$HOME/Documents/dotfiles/shell

export JAVA_HOME=/usr/lib/jvm/default

export ZSH=$HOME/.oh-my-zsh
export FZF_DEFAULT_COMMAND='fd --type f'

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
    if [[ -e ~/.ssh/id_rsa_lw ]]; then
        echo "Adding id_rsa_lw..."
        ssh-add ~/.ssh/id_rsa_lw
    fi
    echo "Done."
fi

# ############################################################################
# Fuzzy finding

[[ -e /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
[[ -e /usr/share/fzf/completion.zsh ]]   && . /usr/share/fzf/completion.zsh

[[ -e $HOME/.fzf/shell/key-bindings.zsh ]] &&  . $HOME/.fzf/shell/key-bindings.zsh
[[ -e $HOME/.fzf/shell/completion.zsh ]] &&  . $HOME/.fzf/shell/completion.zsh

# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

plugins=(
    archlinux
    colored-man-pages
    colorize
    cp
    git
    fzf
    lein
    autojump
)

 [[ -s /home/lsund/.autojump/etc/profile.d/autojump.sh ]] && source /home/lsund/.autojump/etc/profile.d/autojump.sh

autoload -U compinit && compinit -u

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export NNN_USE_EDITOR=1


function precmd() {
    # Log the time of each executed command
    export ZSH_ELAPSED_TIME=$(date '+%s')
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

. $HOME/.aliases
# . $HOME/.widgets

bindkey '^x^n' fuzzy-git-branch
