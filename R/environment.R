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

# from rlglue.types import Action
# from rlglue.types import Observation
# from rlglue.types import Reward_observation_terminal
# 
# class Environment:
#   # () -> string
#   def env_init():
#   pass
# 
# # () -> Observation
# def env_start():
#   pass
# 
# # (Action) -> Reward_observation_terminal
# def env_step(action):
#   pass
# 
# # () -> void
# def env_cleanup():
#   pass
# 
# # (string) -> string
# def env_message(message):
#   pass


# import sys
# 
# import rlglue.network.Network as Network
# from rlglue.types import Action
# from rlglue.types import Observation
# 
# class ClientEnvironment:
#   kUnknownMessage = "Unknown Message: "
# network = None
# env = None
# 
# # (agent) -> void
# def __init__(self, environment):
#   self.env = environment
# self.network = Network.Network()
# 
# # () -> void
# def onEnvInit(self):
#   taskSpec = self.env.env_init()
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvInit)
# self.network.putInt(len(taskSpec) + 4) # Also including the length put in by putString
# self.network.putString(taskSpec)
# 
# # () -> void
# def onEnvStart(self):
#   observation = self.env.env_start()
# size = self.network.sizeOfObservation(observation)
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvStart)
# self.network.putInt(size)
# self.network.putObservation(observation)
# 
# # () -> void
# def onEnvStep(self):
#   action = self.network.getAction()
# reward_observation = self.env.env_step(action)
# size = self.network.sizeOfRewardObservation(reward_observation)
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvStep)
# self.network.putInt(size)
# self.network.putRewardObservation(reward_observation)
# 
# # () -> void
# def onEnvCleanup(self):
#   self.env.env_cleanup()
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvCleanup)
# self.network.putInt(0) # No data in this packet
# 
# # () -> void
# def onEnvMessage(self):
#   message = self.network.getString()
# reply = self.env.env_message(message)
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvMessage)
# if reply == None:
#   #Brian Tanner added payload even for empty message (IE: send that the size is 0)
#   self.network.putInt(4)
# self.network.putInt(0)
# else:
#   #Brian Tanner, added 4 to the payload size because we putString sends the string AND its size
#   self.network.putInt(len(reply) + 4)
# self.network.putString(reply)
# 
# # (string, int, int) -> void
# def connect(self, host, port, timeout):
#   self.network.connect(host, port, timeout);
# self.network.clearSendBuffer()
# self.network.putInt(Network.kEnvironmentConnection)
# self.network.putInt(0) # No body to this packet
# self.network.send()
# 
# # () -> void
# def close(self):
#   self.network.close()
# 
# # () -> void
# def runEnvironmentEventLoop(self):
#   envState = 0
# dataSize = 0
# recvSize = 0
# remaining = 0
# 
# while envState != Network.kRLTerm:
#   self.network.clearRecvBuffer();
# recvSize = self.network.recv(8) - 8; # We may have received the header and part of the payload
# # We need to keep track of how much of the payload was recv'd
# envState = self.network.getInt()
# dataSize = self.network.getInt()
# 
# remaining = dataSize - recvSize;
# if (remaining < 0):
#   print("Remaining was less than 0!")
# remaining = 0
# 
# amountReceived = self.network.recv(remaining)
# 
# # Already read the header, discard it
# self.network.getInt()
# self.network.getInt()
# 
# switch = {
#   Network.kEnvInit: lambda self: self.onEnvInit(),
#   Network.kEnvStart: lambda self: self.onEnvStart(),
#   Network.kEnvStep: lambda self: self.onEnvStep(),
#   Network.kEnvCleanup: lambda self: self.onEnvCleanup(),
#   Network.kEnvMessage: lambda self: self.onEnvMessage() }
# if envState in switch:
#   switch[envState](self)
# elif envState == Network.kRLTerm:
#   pass
# else:
#   sys.stderr.write(Network.kUnknownMessage % (str(envState)))
# sys.exit(1)
# 
# self.network.send()


# import sys
# import os
# import rlglue.network.Network as Network
# from ClientEnvironment import ClientEnvironment
# 
# 
# from rlglue.versions import get_svn_codec_version
# from rlglue.versions import get_codec_version
# 
# def loadEnvironment(theEnvironment):
#   theSVNVersion=get_svn_codec_version()
# theCodecVersion=get_codec_version()
# client = ClientEnvironment(theEnvironment)
# 
# host = Network.kLocalHost
# port = Network.kDefaultPort
# 
# hostString = os.getenv("RLGLUE_HOST")
# portString = os.getenv("RLGLUE_PORT")
# 
# if (hostString != None):
#   host = hostString
# 
# try:
#   port = int(portString)
# except TypeError:
#   port = Network.kDefaultPort
# 
# print "RL-Glue Python Environment Codec Version: "+theCodecVersion+" (Build "+theSVNVersion+")"
# print "\tConnecting to " + host + " on port " + str(port) + "..."
# sys.stdout.flush()
# 
# client.connect(host, port, Network.kRetryTimeout)
# print "\t Environment Codec Connected"
# 
# client.runEnvironmentEventLoop()
# client.close()
# 
# def loadEnvironmentLikeScript():
#   #Assumes you've already done the checking that the number of args and such is good
#   envModule = __import__(sys.argv[1])
# envClass = getattr(envModule,sys.argv[1])
# env = envClass()
# 
# loadEnvironment(env)
