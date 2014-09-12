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

# RL-Glue needs to know what type of object is trying to connect.
kExperimentConnection  = 1
kAgentConnection       = 2
kEnvironmentConnection = 3

kAgentInit    = 4 # agent_* start by sending one of these values
kAgentStart   = 5 # to the client to let it know what type of
kAgentStep    = 6 # event to respond to
kAgentEnd     = 7
kAgentCleanup = 8
kAgentMessage = 10

kEnvInit          = 11
kEnvStart         = 12
kEnvStep          = 13
kEnvCleanup       = 14
kEnvMessage       = 19

kRLInit           = 20
kRLStart          = 21
kRLStep           = 22
kRLCleanup        = 23
kRLReturn         = 24
kRLNumSteps       = 25
kRLNumEpisodes    = 26
kRLEpisode        = 27
kRLAgentMessage   = 33
kRLEnvMessage     = 34

kRLTerm           = 35

kLocalHost = "127.0.0.1"
kDefaultPort = 4096
kRetryTimeout = 2

kDefaultBufferSize = 4096
kIntSize = 4
kDoubleSize = 8
kCharSize = 1

kUnknownMessage = "Unknown Message: %s\n"

Network <- R6Class("Network",
                       public = list(
                          sock = NA, 
                          recvBuffer = '',
                          sendBuffer = '', 
                          connect = function(host=kLocalHost, port=kDefaultPort, retryTimeout=kRetryTimeout) {
                           print(paste("Trying to connect to server."))
                           while(is.na(self$sock)) {
                             (result <- try({
                               self$sock <- socketConnection(host=host, port=port)
                             }))
                             if(all(class(result) == "try-error")) {
                               self$sock <- NA
                               Sys.sleep(retryTimeout)
                             }
                             print("Connection successful.")
                           }
                         },
                         disconnect = function() {
                           try({
                             if(!is.na(self$sock) && isOpen(self$sock)) {
                                close(self$sock)
                                self$sock <- NA
                             }
                           })
                         },
                         send = function() {
                           if(is.na(self$sock) || !isOpen(self$sock)) self$connect()
                           write(self$sendBuffer, self$sock)
                         },
                         recv = function(size) {
                           s = ''
                           if(is.na(self$sock) || !isOpen(self$sock)) self$connect()
                           while(nchar(s) < size) {
                             x <- readLines(self$sock)
                             self$recvBuffer <- paste(recvBuffer, x, sep='')
                           }
                           return(nchar(self$recvBuffer))
                         },
                         clearSendBuffer = function() self$sendBuffer = '',
                         clearRecvBuffer = function() self$recvBuffer = '',
                         getInt = function() as.integer(self$recvBuffer),
                         getDouble = function() as.double(self$recvBuffer),
                         getChar = function() as.character(self$recvBuffer),
                         getAbstractType = function() {
                           at <- list()
                           at$intArray = getInt()
                           at$doubleArray = getDouble()
                           at$charArray = getChar()
                           return(at)
                         },
                         putInt = function(value) self$sendBuffer = paste(self$sendBuffer, value),
                         putDouble = function(value) self$sendBuffer = paste(self$sendBuffer, value),
                         putChar = function(value) self$sendBuffer = paste(self$sendBuffer, value)
                       )
)



# def flipSendBuffer(self):
#   self.clearSendBuffer()
# 
# def flipRecvBuffer(self):
#   self.clearRecvBuffer()
# 
# 
# def getObservation(self):
#   return Observation.fromAbstractType(self.getAbstractType())
# 
# def getAction(self):
#   return Action.fromAbstractType(self.getAbstractType())
# 
# 
# def putObservation(self,obs):
#   self.putAbstractType(obs)
# 
# def putAction(self,action):
#   self.putAbstractType(action)
# 
# def putAbstractType(self, theItem):
#   self.putInt(len(theItem.intArray))
# self.putInt(len(theItem.doubleArray))
# self.putInt(len(theItem.charArray))
# if len(theItem.intArray) > 0:
#   self.sendBuffer.write(struct.pack("!%di" % (len(theItem.intArray)),*(theItem.intArray)))
# if len(theItem.doubleArray) > 0:
#   self.sendBuffer.write(struct.pack("!%dd" % (len(theItem.doubleArray)),*(theItem.doubleArray)))
# if len(theItem.charArray) > 0:
#   self.sendBuffer.write(struct.pack("!%dc" % (len(theItem.charArray)),*(theItem.charArray)))
# 
# def putRewardObservation(self,rewardObservation):
#   self.putInt(rewardObservation.terminal);
# self.putDouble(rewardObservation.r);
# self.putObservation(rewardObservation.o);
# 
# 
# def sizeOfAbstractType(self, theItem):
#   size = kIntSize * 3
# intSize = 0
# doubleSize = 0
# charSize = 0
# if theItem != None:
#   if theItem.intArray != None:
#   intSize = kIntSize * len(theItem.intArray)
# if theItem.doubleArray != None:
#   doubleSize = kDoubleSize * len(theItem.doubleArray)
# if theItem.charArray != None:
#   charSize = kCharSize * len(theItem.charArray)
# return size + intSize + doubleSize + charSize
# 
# 
# def sizeOfAction(self,action):
#   return self.sizeOfAbstractType(action)
# 
# def sizeOfObservation(self,observation):
#   return self.sizeOfAbstractType(observation)
# 
# def sizeOfRewardObservation(self,reward_observation):
#   return kIntSize + kDoubleSize + self.sizeOfObservation(reward_observation.o)
