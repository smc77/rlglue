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
# 
# list(sock = "connection",
#      recvBuffer="character", 
#      sendBuffer="character", 
#      getAbstractType="list")
kUnknownMessage = "Unknown Message: %s\n"

Network <- setRefClass("Network",
                       fields = c("sock", "recvBuffer", "sendBuffer", "getAbstractType"),
                       methods = list(
                         connect = function(host=kLocalHost, port=kDefaultPort, retryTimeout=kRetryTimeout) {
                           print(paste("Trying to connect to server."))
                           while(is.na(sock)) {
                             (result <- try({
                               sock <<- socketConnection(host=host, port=port)
                             }))
                             if(all(class(result) == "try-error")) {
                               sock <<- NA
                               Sys.sleep(retryTimeout)
                             }
                             print("Connection successful.")
                           }
                         }
                       )
)

network <- function() {
  n <- list(sock=NA, recvBuffer='', sendBuffer='', getAbstractType='list')
  class(n) <- "network"
  return(n)
}

is.network <- function(n) class(n) == "network"

net.connect <- function(network, host=kLocalHost, port=kDefaultPort, retryTimeout=kRetryTimeout) {
  if(missing(network) || !is.network(network)) stop("Need to provide a valid network object.")
  orig.name <- as.character(match.call()$network)
  print(paste("Trying to connect to server for", orig.name, "."))
  while(is.na(network$sock)) {
    (result <- try({
      network$sock = socketConnection(host=host, port=port)
    }))
    if(all(class(result) == "try-error")) {
      network$sock <- NA
      Sys.sleep(retryTimeout)
    }
    print("Connection successful.")
    assign(orig.name, network, parent.frame(1))     
  } 
}

net.close <- function(network) {
  orig.name <- as.character(match.call()$network)
  sock <- network$sock
  if(isOpen(sock)) {
    flush(sock)
    close(sock)
    network$sock <- NA
    assign(orig.name, network, parent.frame(1))     
  }
}

net.send <- function(network) {
  orig.name <- as.character(match.call()$network)
  if(!isOpen(network$sock)) net.connect(network)
  write(network$sendBuffer, network$sock)
  assign(orig.name, network, parent.frame(1))     
}

# def close(self):
#   self.sock.close()
# 
# def send(self):
#   self.sock.sendall(self.sendBuffer.getvalue())
# 
# def recv(self,size):
#   s = ''
# while len(s) < size:
#   s += self.sock.recv(size - len(s))
# self.recvBuffer.write(s)
# self.recvBuffer.seek(0)
# return len(s)
# 
# def clearSendBuffer(self):
#   self.sendBuffer.close()
# self.sendBuffer = StringIO.StringIO()
# 
# def clearRecvBuffer(self):
#   self.recvBuffer.close()
# self.recvBuffer = StringIO.StringIO()
# 
# def flipSendBuffer(self):
#   self.clearSendBuffer()
# 
# def flipRecvBuffer(self):
#   self.clearRecvBuffer()
# 
# def getInt(self):
#   s = self.recvBuffer.read(kIntSize)
# return struct.unpack("!i",s)[0]
# 
# def getDouble(self):
#   s = self.recvBuffer.read(kDoubleSize)
# return struct.unpack("!d",s)[0]
# 
# def getString(self):
#   #If you read 0 you get "" not None so that's fine
#   length = self.getInt()
# return self.recvBuffer.read(length)
# 
# 
# def getAbstractType_list(self):
#   numInts = self.getInt()
# numDoubles = self.getInt()		
# numChars = self.getInt()		
# returnStruct=RL_Abstract_Type()
# 
# if numInts > 0:
#   s = self.recvBuffer.read(numInts*kIntSize)
# returnStruct.intArray = list(struct.unpack("!%di" % (numInts),s))
# if numDoubles > 0:
#   s = self.recvBuffer.read(numDoubles*kDoubleSize)
# returnStruct.doubleArray = list(struct.unpack("!%dd" % (numDoubles),s))
# if numChars > 0:
#   s = self.recvBuffer.read(numChars*kCharSize)
# returnStruct.charArray = list(struct.unpack("!%dc" % (numChars),s))
# return returnStruct
# 
# def getAbstractType_numpy(self):
#   numInts = self.getInt()
# numDoubles = self.getInt()		
# numChars = self.getInt()		
# returnStruct=RL_Abstract_Type()
# 
# if numInts > 0:
#   s = self.recvBuffer.read(numInts*kIntSize)
# assert kIntSize == 4
# returnStruct.intArray = numpy.frombuffer(s,
#                                          dtype=numpy_int_type,
#                                          count=numInts)
# if numDoubles > 0:
#   s = self.recvBuffer.read(numDoubles*kDoubleSize)
# returnStruct.doubleArray = numpy.frombuffer(s, count=numDoubles,
#                                             dtype=numpy_float_type)
# if numChars > 0:
#   s = self.recvBuffer.read(numChars*kCharSize)
# returnStruct.charArray = numpy.frombuffer(s, count=numChars,
#                                           dtype=numpy_char_type)
# return returnStruct
# 
# def getObservation(self):
#   return Observation.fromAbstractType(self.getAbstractType())
# 
# def getAction(self):
#   return Action.fromAbstractType(self.getAbstractType())
# 
# def putInt(self,value):
#   self.sendBuffer.write(struct.pack("!i",value))
# 
# def putDouble(self,value):
#   self.sendBuffer.write(struct.pack("!d",value))
# 
# def putString(self,value):
#   if value == None:
#   value = ''
# self.putInt(len(value))
# self.sendBuffer.write(value)
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
