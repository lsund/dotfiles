# ssh-agent-procure.bash
# v0.6.4
# ensures that all shells sourcing this file in profile/rc scripts use the same ssh-agent.
# copyright me, now; licensed under the DWTFYWT license.

mkdir -p "$HOME/etc/ssh";

function ssh-procure-launch-agent {
    eval `ssh-agent -s -a ~/etc/ssh/ssh-agent-socket`;
    ssh-add;
}

if [ ! $SSH_AGENT_PID ]; then
  if [ -e ~/etc/ssh/ssh-agent-socket ] ; then
    SSH_AGENT_PID=`ps aux |grep ssh-agent | grep -v grep | awk '{print $2}'`;
    if [[ $SSH_AGENT_PID =~ [0-9]+ ]]; then
      # in this case the agent has already been launched and we are just attaching to it.
      ##++  It should check that this pid is actually active & belongs to an ssh instance
      export SSH_AGENT_PID;
      SSH_AUTH_SOCK=~/etc/ssh/ssh-agent-socket; export SSH_AUTH_SOCK;
    else
      # in this case there is no agent running, so the socket file is left over from a graceless agent termination.
      rm ~/etc/ssh/ssh-agent-socket;
      ssh-procure-launch-agent;
    fi;
  else
    ssh-procure-launch-agent;
  fi;
fi;
