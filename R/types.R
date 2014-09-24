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

RL_Abstract_Type <- R6Class("RL_Abstract_Type",
                 public=list(
                   intArray = c(),
                   doubleArray = c(),
                   charArray = c(),
                   initialize = function(numInts=NA, numDoubles=NA, numChars=NA) {
                     if(!is.na(numInts)) self$intArray <- rep(0L, numInts)
                     if(!is.na(numDoubles)) self$doubleArray <- rep(0, numDoubles)
                     if(!is.na(numChars)) self$charArray <- rep('', numChars)
                   },
                   sameAs = function(otherAbstractType) {
                     return(length(self$intArray) == length(otherAbstractType$intArray) && 
                              length(self$doubleArray) == length(otherAbstractType$doubleArray) && 
                              length(self$charArray) == length(otherAbstractType$charArray) && 
                              all(self$intArray == otherAbstractType$intArray) && 
                              all(self$doubleArray == otherAbstractType$doubleArray) && 
                              all(self$charArray == otherAbstractType$charArray))
                   }
                 ))

Action <- R6Class("Action",
                  inherit = RL_Abstract_Type,
                  public=list(
                    fromAbstractType = function(AbstractType) {
                      self$intArray = AbstractType$intArray
                      self$doubleArray = AbstractType$doubleArray
                      self$charArray = AbstractType$charArray
                    }))

Observation <- R6Class("Observation",
                  inherit = RL_Abstract_Type,
                  public=list(
                    fromAbstractType = function(AbstractType) {
                      self$intArray = AbstractType$intArray
                      self$doubleArray = AbstractType$doubleArray
                      self$charArray = AbstractType$charArray
                    }))

Observation_action <- R6Class("Observation_action",
                              public = list(
                                o=Observation$new(),
                                a=Action$new(),
                                initialize=function(theObservation=NULL,theAction=NULL) {
                                  if(!all(is.null(theObservation))) self$o = theObservation
                                  if(!all(is.null(theAction))) self$a = theAction
                                })
)

Reward_observation_terminal <- R6Class("Reward_observation_terminal",
                              public = list(
                                r = 0.0,
                                o=Observation$new(),
                                terminal=FALSE,
                                initialize=function(reward=NULL, theObservation=NULL, terminal=NULL) {
                                  if(!all(is.null(reward))) self$r = reward
                                  if(!all(is.null(theObservation))) self$o = theObservation
                                  if(!all(is.null(terminal))) self$terminal = terminal
                                })
)


Reward_observation_action_terminal <- R6Class("Reward_observation_action_terminal",
                                       public = list(
                                         r = 0.0,
                                         o=Observation$new(),
                                         a=Action$new(),
                                         terminal=FALSE,
                                         initialize=function(reward=NULL, theObservation=NULL, theAction=NULL, terminal=NULL) {
                                           if(!all(is.null(reward))) self$r = reward
                                           if(!all(is.null(theObservation))) self$o = theObservation
                                           if(!all(is.null(theAction))) self$a = theAction
                                           if(!all(is.null(terminal))) self$terminal = terminal
                                         })
)
