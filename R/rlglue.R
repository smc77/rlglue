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

forceConnection <- function() {
  theCodecVersion = packageVersion("rlglue")
  
  host = kLocalHost
  port = kDefaultPort
  
  #
  hostString = Sys.getenv("RLGLUE_HOST")
  portString = Sys.getenv("RLGLUE_PORT")
  
  print(paste("RL-Glue R Experiment Codec Version:", theCodecVersion, "."))
  print(paste("Connecting to ", host, " on port ", port, "...", sep=""))
  
  rlglue.network <<- Network$new()
  rlglue.network$connect(host, port)
  rlglue.network$clearSendBuffer()
  rlglue.network$putInt(kExperimentConnection)
  rlglue.network$putInt(0)
  rlglue.network$send()
}

doStandardRecv <- function(state) {
  rlglue.network$clearRecvBuffer()
  recvSize = rlglue.network$recv(8) - 8

  if(!length(rlglue.network$recvBuffer)) return(FALSE)
  
  glueState = rlglue.network$getInt()
  dataSize = rlglue.network$getInt()
  remaining = dataSize - recvSize

  if(!length(glueState) || !length(dataSize)) return(FALSE) 
  if(remaining < 0) remaining = 0
  
  remainingReceived = rlglue.network$recv(remaining)
  
  # Already read the header, so discard it
  #rlglue.network$getInt()
  #rlglue.network$getInt()
  
  if (!length(glueState) || !length(dataSize) || glueState != state) {
    print(paste("Not synched with server. glueState = ", str(glueState), " but should be ", state, "\n"), sep="")
    return(FALSE)
  }
  return(TRUE)
}

doCallWithNoParams <- function(state) {
  rlglue.network$clearSendBuffer()
  rlglue.network$putInt(state)
  rlglue.network$putInt(0)
  rlglue.network$send()
}

RL_setup <- function(val) {
  doCallWithNoParams(val)
  doStandardRecv(val)
}

RL_init <- function(recurse=TRUE) {
  forceConnection()
  if(!RL_setup(kRLInit)) 
    if(!RL_setup(kRLInit)) return("")
  #doCallWithNoParams(kRLInit)
  #doStandardRecv(kRLInit)
  taskSpecResponse = rlglue.network$getString()
  return(taskSpecResponse)
}

RL_start <- function() {
  obsact = NA
  if(!RL_setup(kRLStart)) RL_setup(kRLStart)
  obsact = Observation_action$new()
  obsact$o = rlglue.network$getObservation()
  obsact$a = rlglue.network$getAction()
  return(obsact)
}

RL_step <- function(){
  if(!RL_setup(kRLStep)) RL_setup(kRLStep)
  roat = Reward_observation_action_terminal$new()
  roat$terminal = rlglue.network$getInt()
  roat$r = rlglue.network$getDouble()
  roat$o = rlglue.network$getObservation()
  roat$a = rlglue.network$getAction()
  return(roat)
}

RL_cleanup <- function() {
  if(!RL_setup(kRLCleanup)) RL_setup(kRLCleanup)
}

RL_agent_message <- function(message) {
  if(is.na(message)) 
    message = ""
  response = ""
  forceConnection()
  rlglue.network$clearSendBuffer()
  rlglue.network$putInt(kRLAgentMessage)
  #Payload Size
  rlglue.network$putInt(nchar(message) + 4)
  rlglue.network$putString(message)
  rlglue.network$send()
  doStandardRecv(NkRLAgentMessage)
  response = rlglue.network$getString()
  return(response)
}

RL_env_message <- function(message) {
  if(is.na(message)) 
    message = ""
  response = ""
  forceConnection()
  rlglue.network$clearSendBuffer()
  rlglue.network$putInt(kRLEnvMessage)
  #Payload Size
  rlglue.network$putInt(nchar(message) + 4)
  rlglue.network$putString(message)
  rlglue.network$send()
  doStandardRecv(kRLEnvMessage)
  response = rlglue.network$getString()
  return(response)
}

RL_return <- function() {
  reward = 0.0
  if(!RL_setup(kRLReturn)) RL_setup(kRLReturn)
  reward = rlglue.network$getDouble()
  return(reward)
}

RL_num_steps <- function() {
  numSteps = 0
  if(!RL_setup(kRLNumSteps)) RL_setup(kRLNumSteps)
  numSteps = rlglue.network$getInt()
  return(numSteps)
}

RL_num_episodes <- function() {
  numEpisodes = 0
  if(!RL_setup(kRLNumEpisodes)) RL_setup(kRLNumEpisodes)
  numEpisodes = rlglue.network$getInt()
  return(numEpisodes)
}

RL_episode <- function(num_steps) {
  rlglue.network$clearSendBuffer()
  rlglue.network$putInt(kRLEpisode)
  rlglue.network$putInt(kIntSize)
  rlglue.network$putInt(num_steps)
  rlglue.network$send()
  doStandardRecv(kRLEpisode)
  exitStatus = rlglue.network$getInt()
  return(exitStatus)
}
