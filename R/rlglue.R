# 
# Copyright (C) 2014, Shane Conway
# 
# Based on similar codecs, primarily written by Brian Tanner, and the Python extension mostly written by Mark Lee:
# http://rl-glue-ext.googlecode.com/
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

force_connection <- function() {
  if(is.na(rlglue.network)) {
    
    theCodecVersion = packageVersion("rlglue")
    
    host = kLocalHost
    port = kDefaultPort
    
    #
    hostString = Sys.getenv("RLGLUE_HOST")
    portString = Sys.getenv("RLGLUE_PORT")
    
    print(paste("RL-Glue R Experiment Codec Version:", theCodecVersion, "."))
    print(paste("\tConnecting to ", host, " on port ", port, "...", sep=""))
    
    rlglue.network <<- Network$new()
    rlglue.network$connect(host, port)
    rlglue.network$clearSendBuffer()
    rlglue.network$putInt(kExperimentConnection)
    rlglue.network$putInt(0)
    rlglue.network$send()
  }
}

doStandardRecv <- function(state) {
  rlglue.network$clearRecvBuffer()
  recvSize = rlglue.network$recv(8) - 8
  
  glueState = rlglue.network$getInt()
  dataSize = rlglue.network$getInt()
  remaining = dataSize - recvSize
  
  if(remaining < 0) remaining = 0
  
  remainingReceived = rlglue.network$recv(remaining)
  
  # Already read the header, so discard it
  rlglue.network$getInt()
  rlglue.network$getInt()
  
  if (glueState != state) {
    stop(paste("Not synched with server. glueState = ", str(glueState), " but should be ", state, "\n"), sep="")
  }
  
}

doCallWithNoParams <- function(state) {
  
}

# # (int) -> void
# def doCallWithNoParams(state):
#   network.clearSendBuffer()
# network.putInt(state)
# network.putInt(0)
# network.send()
# 
# #Brian Tanner... need to make this return a string
# # () -> string
# def RL_init():
#   forceConnection()
# doCallWithNoParams(Network.kRLInit)
# doStandardRecv(Network.kRLInit)
# #Brian Tanner added
# taskSpecResponse = network.getString()
# return taskSpecResponse
# 
# # () -> Observation_action
# def RL_start():
#   obsact = None
# doCallWithNoParams(Network.kRLStart)
# doStandardRecv(Network.kRLStart)
# obsact = Observation_action()
# obsact.o = network.getObservation()
# obsact.a = network.getAction()
# return obsact
# 
# # () -> Reward_observation_action_terminal
# def RL_step():
#   roat = None
# doCallWithNoParams(Network.kRLStep)
# doStandardRecv(Network.kRLStep)
# roat = Reward_observation_action_terminal()
# roat.terminal = network.getInt()
# roat.r = network.getDouble()
# roat.o = network.getObservation()
# roat.a = network.getAction()
# return roat
# 
# # () -> void
# def RL_cleanup():
#   doCallWithNoParams(Network.kRLCleanup)
# doStandardRecv(Network.kRLCleanup)
# 
# # (string) -> string
# def RL_agent_message(message):
#   if message == None:
#   message=""
# response = ""
# forceConnection()
# network.clearSendBuffer()
# network.putInt(Network.kRLAgentMessage)
# #Payload Size
# network.putInt(len(message) + 4)
# network.putString(message)
# network.send()
# doStandardRecv(Network.kRLAgentMessage)
# response = network.getString()
# return response
# 
# # (string) -> string
# def RL_env_message(message):
#   if message == None:
#   message=""
# response = ""
# forceConnection()
# network.clearSendBuffer()
# network.putInt(Network.kRLEnvMessage)
# #Payload Size
# network.putInt(len(message) + 4)
# network.putString(message)
# network.send()
# doStandardRecv(Network.kRLEnvMessage)
# response = network.getString()
# return response
# 
# # () -> double
# def RL_return():
#   reward = 0.0
# doCallWithNoParams(Network.kRLReturn)
# doStandardRecv(Network.kRLReturn)
# reward = network.getDouble()
# return reward
# 
# # () -> int
# def RL_num_steps():
#   numSteps = 0
# doCallWithNoParams(Network.kRLNumSteps)
# doStandardRecv(Network.kRLNumSteps)
# numSteps = network.getInt()
# return numSteps
# 
# # () -> int
# def RL_num_episodes():
#   numEpisodes = 0
# doCallWithNoParams(Network.kRLNumEpisodes)
# doStandardRecv(Network.kRLNumEpisodes)
# numEpisodes = network.getInt()
# return numEpisodes
# 
# #Brian Tanner needs to make this return an int
# # (int) -> int
# def RL_episode(num_steps):
#   network.clearSendBuffer()
# network.putInt(Network.kRLEpisode)
# network.putInt(Network.kIntSize)
# network.putInt(num_steps)
# network.send()
# doStandardRecv(Network.kRLEpisode)
# #Brian Tanner added
# exitStatus = network.getInt()
# return exitStatus
