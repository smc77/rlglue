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

abstract.type <- function(numInts=NA, numDoubles=NA, numChars=NA) {
  atype <- list(intArray=c(), doubleArray=c(), charArray=c())
  
  if(!is.na(numInts)) atype$intArray <- rep(0L, numInts)
  if(!is.na(numDoubles)) atype$doubleArray <- rep(0, numDoubles)
  if(!is.na(numChars)) atype$charArray <- rep('', numChars)
  
  return(atype)
}

action <- function(numInts=NA, numDoubles=NA, numChars=NA) {
  atype <- abstract.type(numInts=numInts, numDoubles=numDoubles, numChars=numChars)
  class(atype) <- "action"
  return(atype)
}

observation <- function(numInts=NA, numDoubles=NA, numChars=NA) {
  atype <- abstract.type(numInts=numInts, numDoubles=numDoubles, numChars=numChars)
  class(atype) <- "observation"
  return(atype)
}

# class Observation_action:
#   def __init__(self,theObservation=None,theAction=None):
#   if theObservation != None:
#   self.o = theObservation
# else:
#   self.o = Observation()
# if theAction != None:
#   self.a = theAction
# else:
#   self.a = Action()
# 
# class Reward_observation_terminal:
#   def __init__(self,reward=None, theObservation=None, terminal=None):
#   if reward != None:
#   self.r = reward
# else:
#   self.r = 0.0
# if theObservation != None:
#   self.o = theObservation
# else:
#   self.o = Observation()
# if terminal != None:
#   self.terminal = terminal
# else:
#   self.terminal = False
# 
# class Reward_observation_action_terminal:
#   def __init__(self,reward=None, theObservation=None, theAction=None, terminal=None):
#   if reward != None:
#   self.r = reward
# else:
#   self.r = 0.0
# if theObservation != None:
#   self.o = theObservation
# else:
#   self.o = Observation()
# if theAction != None:
#   self.a = theAction
# else:
#   self.a = Action()
# if terminal != None:
#   self.terminal = terminal
# else:
#   self.terminal = False
# 
# 
