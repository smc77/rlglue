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
                          recvBuffer = list(),
                          sendBuffer = list(), 
                          connect = function(host=kLocalHost, port=kDefaultPort, retryTimeout=kRetryTimeout) {
                           print(paste("Trying to connect to server."))
                           while(is.na(self$sock)) {
                             (result <- try({
                               self$sock <- socketConnection(host=host, port=port, open="ab")
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
                           lapply(self$sendBuffer, function(x) writeBin(x, self$sock))
                         },
                         recv = function(size) {
                           if(is.na(self$sock) || !isOpen(self$sock)) self$connect()
                           while(length(self$recvBuffer) < size) {
                             x <- readBin(self$sock, raw(), size=1, endian="big")#readLines(self$sock)
                             self$recvBuffer <- c(self$recvBuffer, list(x))
                           }
                           return(length(self$recvBuffer))
                         },
                         clearSendBuffer = function() self$sendBuffer = list(),
                         clearRecvBuffer = function() self$recvBuffer = list(),
                         getInt = function() {
                           i <- as.integer(readBin(unlist(self$recvBuffer[1:4]), integer(), endian="big"))
                           self$recvBuffer <- self$recvBuffer[-c(1:4)]
                           return(i)
                         },
                         getDouble = function() {
                           d <- as.double(readBin(unlist(self$cvBuffer[1:8]), double(), endian="big"))
                           self$recvBuffer <- self$recvBuffer[-c(1:8)]
                           return(d)
                         },
                         getString = function() {
                           size = self$getInt()
                           readChar(unlist(self$recvBuffer), size)
                         },
                         getAbstractType = function() {
                           numInts = self$getInt()
                           numDoubles = self$getInt()
                           numChars = self$getInt()
                           returnStruct = RL_Abstract_Type$new()
                           
                           if(numInts > 0) {
                             returnStruct$intArray = lapply(1:numInts, self$getInt(s))
                           }
                           if(numDoubles > 0) {
                             returnStruct$doubleArray = lapply(1:numDoubles, self$getDouble())
                           }
                           if(numChars > 0) {
                             returnStruct$charArray = lapply(1:numChars, self$getString())
                           }
                           return(returnStruct)
                         },
                         getObservation = function() {
                           Observation$new()$fromAbstractType(self$getAbstractType())
                         },
                         getAction = function() {
                           Action$new()$fromAbstractType(self$getAbstractType())
                         },
                         putInt = function(value) self$sendBuffer <- c(self$sendBuffer, list(writeBin(as.integer(value), con=raw(), size=4L, endian="big"))),
                         putDouble = function(value) self$sendBuffer <- c(self$sendBuffer, list(writeBin(as.double(value), con=raw(), size=8L, endian="big"))),
                         putChar = function(value) self$sendBuffer = c(self$sendBuffer, list(writeBin(as.character(value), con=raw(), size=8L, endian="big"))),
                         putAbstractType = function(theItem) {
                           self$putInt(length(theItem$intArray))
                           self$putInt(length(theItem$doubleArray))
                           self$putInt(length(theItem$charArray))
                           if (length(theItem$intArray) > 0) {
                             lapply(theItem$intArray, function(i) self$putInt(i))
                           }
                           if (length(theItem$doubleArray) > 0) {
                             lapply(theItem$doubleArray, function(d) self$putDouble(d))
                           }
                           if (length(theItem$charArray) > 0) {
                             lapply(theItem$charArray, function(c) self$putChar(c))
                           }
                         },
                         putObservation = function(obs) {
                           self$putAbstractType(obs)
                         }, 
                         putAction = function(action) {
                           self$putAbstractType(action)
                         }, 
                         putRewardObservation = function(rewardObservation) {
                           self$putInt(rewardObservation$terminal)
                           self$putDouble(rewardObservation$r)
                           self$putObservation(rewardObservation$o)
                         },
                         sizeOfAbstractType(theItem) {
                           size = kIntSize * 3
                           intSize = 0
                           doubleSize = 0
                           charSize = 0
                           if(!is.na(theItem) {
                             if(!is.na(theItem$intArray)) {
                               intSize = kIntSize * length(theItem$intArray)
                             }
                             if(!is.na(theItem$doubleArray)) {
                               doubleSize = kDoubleSize * length(theItem$doubleArray)
                             }
                             if(!is.na(theItem$charArray)) {
                               charSize = kCharSize * length(theItem$charArray)
                             }
                           }
                           return size + intSize + doubleSize + charSize
                         },
                         sizeOfAction <- function(action) {
                           self$sizeOfAbstractType(action)
                         },
                         sizeOfObservation <- function(observation) {
                           self$sizeOfAbstractType(observation)
                         },
                         sizeOfRewardObservation <- function(reward_observation) {
                           return kIntSize + kDoubleSize + self$sizeOfObservation(reward_observation$o)
                         }
                       )
)
