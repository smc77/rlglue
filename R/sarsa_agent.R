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

# 
# import random
# import sys
# import copy
# import pickle
# from rlglue.agent.Agent import Agent
# from rlglue.agent import AgentLoader as AgentLoader
# from rlglue.types import Action
# from rlglue.types import Observation
# from rlglue.utils import TaskSpecVRLGLUE3
# from random import Random
# 


# This is a very simple Sarsa agent for discrete-action, discrete-state
# environments.  It uses epsilon-greedy exploration.
# 
# We've made a decision to store the previous action and observation in 
# their raw form, as structures.  This code could be simplified and you
# could store them just as ints.


# TO USE THIS Agent [order doesn't matter]
# NOTE: I'm assuming the Python codec is installed an is in your Python path
#   -  Start the rl_glue executable socket server on your computer
#   -  Run the SampleMinesEnvironment and SampleExperiment from this or a
#   different codec (Matlab, Python, Java, C, Lisp should all be fine)
#   -  Start this agent like:
#   $> python sample_sarsa_agent.py


SarsaAgent <- R6Class("SarsaAgent",
                 inherit=Agent,
                 public=list(
                   randGenerator = rnorm,
                   lastAction = 'action()',
                   lastObservation = 'observation()',
                   sarsa_stepsize = 0.1,
                   sarsa_epsilon = 0.1,
                   sarsa_gamma = 1.0,
                   numStates = 0,
                   numActions = 0,
                   value_function = NA,
                   policyFrozen=FALSE,
                   exploringFrozen=FALSE,
                   agent_init = function(taskSpecString) {
                     TaskSpec <- TaskSpecParser(taskSpecString)
                     if(TaskSpec$valid) {
#                        assert len(TaskSpec.getIntObservations())==1, "expecting 1-dimensional discrete observations"
#                        assert len(TaskSpec.getDoubleObservations())==0, "expecting no continuous observations"
#                        assert not TaskSpec.isSpecial(TaskSpec.getIntObservations()[0][0]), " expecting min observation to be a number not a special value"
#                        assert not TaskSpec.isSpecial(TaskSpec.getIntObservations()[0][1]), " expecting max observation to be a number not a special value"
                       self$numStates=TaskSpec$getIntObservations()[0, 1] + 1
                       
#                        assert len(TaskSpec.getIntActions())==1, "expecting 1-dimensional discrete actions"
#                        assert len(TaskSpec.getDoubleActions())==0, "expecting no continuous actions"
#                        assert not TaskSpec.isSpecial(TaskSpec.getIntActions()[0][0]), " expecting min action to be a number not a special value"
#                        assert not TaskSpec.isSpecial(TaskSpec.getIntActions()[0][1]), " expecting max action to be a number not a special value"
                       self$numActions=TaskSpec$getIntActions()[0, 1] + 1
                       
                       self$value_function = rep(0.0, self$numActions * self$numStates)
                     } else {
                       print(paste("Task Spec could not be parsed:", taskSpecString))
                     }
                     
                     self$lastAction=Action()
                     self$lastObservation=Observation()
                   },
                   egreedy = function(state) {
                     maxIndex=0
                     a=1
                     if(!self$exploringFrozen && self$randGenerator() < self$sarsa_epsilon)
                       return(sample(1:self$numActions, 1))
                     return(1)
                     #return self$value_function[state].index(max(self.value_function[state]))
                   },
                   agent_start = function(observation=NA) {
                     return()
                   },
                   agent_step = function(reward=NA, observation=NA) return(),
                   agent_end = function(reward=NA) return(),
                   agent_cleanup = function() return(),
                   save_value_function = function(fileName) {
                     # Check if the file exists
                     # Save the binary file
                     save(self$value_function, fileNmae)
                   },
                   load_value_function = function(fileName) {
                     # Check if the file exists
                     # Load the binary file
                     val_name <- load(fileNmae)
                     self$value_function <- get(val_name)
                   },
                   agent_message = function(message=NA) return()
                 ))
