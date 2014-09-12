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
# Code for task specification parsing.  Task specification 
#
# More here: http://glue.rl-community.org/wiki/Task_Spec
#

tsp.example <- "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 OBSERVATIONS INTS (UNSPEC 1) ACTIONS DOUBLES (NEGINF POSINF) CHARCOUNT 0 REWARDS (UNSPEC UNSPEC) EXTRA Name: Test Problem A"

assign("rlglue.tsp", list(w=c("VERSION","PROBLEMTYPE","DISCOUNTFACTOR", "OBSERVATIONS","ACTIONS","REWARDS","EXTRA"),
            v=c("INTS","DOUBLES","CHARCOUNT"), 
            expected_version = "RL-Glue-3.0",
            valid = TRUE,
            last_error = ""), envir=.GlobalEnv)


TaskSpec <- R6Class("TaskSpec",
                   public = list(
                     w = c("VERSION","PROBLEMTYPE","DISCOUNTFACTOR", "OBSERVATIONS","ACTIONS","REWARDS","EXTRA"), 
                     v=c("INTS","DOUBLES","CHARCOUNT"), 
                     expected_version = "RL-Glue-3.0",
                     valid = TRUE,
                     last_error = "")
)
                     

rlglue.tsp <- list(w=c("VERSION","PROBLEMTYPE","DISCOUNTFACTOR", "OBSERVATIONS","ACTIONS","REWARDS","EXTRA"),
     v=c("INTS","DOUBLES","CHARCOUNT"), 
     expected_version = "RL-Glue-3.0",
     valid = TRUE,
     last_error = "")

#environment(rlglue.tsp) <- as.environment('package:rl.glue')

tsp.init <- function(ts, tsp=NA) {
  if(!is.na(tsp))
    orig.var.name <- as.character(match.call()$tsp)
  tsp$ts <- ts
  ts2 <- strsplit(ts, " ")[[1]]
  version.index <- ts2 == "VERSION"
  problemtype.index <- ts2 == "PROBLEMTYPE"
  discountfactor.index <- ts2 == "DISCOUNTFACTOR"
  observations.index <- ts2 == "OBSERVATIONS"
  actions.index <- ts2 == "ACTIONS"
  if(tsp$expected_version != ts2[version.index + 1])
    stop("Task specification is for the wrong RL-Glue version")
  tsp$version <- ts2[version.index + 1]
  tsp$valid = FALSE
  assign(orig.var.name, tsp, parent.frame(1))
}

# tsp.getVersion() <- function(tsp) {
#   #   a = len(self.w[0])+1
#   # return self.ts[a:self.ts.find(" ",a)]
#   a = nchar(tsp$w[1])
#   return(tsp$ts[a + 1:])
# }
# 
# 
# 
# 
# def __init__(self,ts):
#   self.ts = ts
# if self.expected_version != self.getVersion():
#   print "Warning: TaskSpec Version is not "+self.expected_version+" but "+self.getVersion()
# self.valid = False
# 
# def getVersion(self):
#   a = len(self.w[0])+1
# return self.ts[a:self.ts.find(" ",a)]
# 
# def Validate(self):
#   if not self.valid:
#   print "Warning: TaskSpec String is invalid: "+self.last_error
# return False
# return True
# 
# def getValue(self,i,ts,w):
#   try:
#   a = ts.index(w[i]) + len(w[i]) + 1
# except: #ValueError:
#   #raise AttributeError("Malformed TaskSpec String: could not find the "+w[i]+" keyword")
#   self.last_error = "could not find the "+w[i]+" keyword"
# print "Warning: Malformed TaskSpec String: " +self.last_error
# self.valid = False
# return ""
# b=None
# if (i+1)<len(w):
#   try:
#   b = ts.index(w[i+1])-1
# except: #ValueError:
#   #raise AttributeError("Malformed TaskSpec String: could not find the "+w[i+1]+" keyword")
#   self.last_error = "could not find the "+w[i+1]+" keyword"
# print "Warning: Malformed TaskSpec String: " +self.last_error
# self.valid = False
# return ""
# 
# return ts[a:b].strip()
# 
# def getProblemType(self):
#   if not self.Validate():
#   return ""
# return self.getValue(1,self.ts,self.w)
# 
# def getDiscountFactor(self):
#   if not self.Validate():
#   return ""
# return float(self.getValue(2,self.ts,self.w))	
# 
# def CompleteVars(self,str_in):
#   if not self.Validate():
#   return ""
# """ forces the vars to have ints doubles and charcount
# """
# if self.v[0] not in str_in:
#   str_in = self.v[0]+" (0 0 0) " + str_in
# if self.v[2] not in str_in:
#   str_in= str_in.rstrip()+" "+self.v[2]+" 0 "
# if self.v[1] not in str_in:
#   i = str_in.find(self.v[2])
# str_in= str_in[0:i]+self.v[1]+" (0 0 0) "+str_in[i:]
# 
# 
# return str_in
# 
# def getObservations(self):
#   if not self.Validate():
#   return ""
# str_o = self.getValue(3,self.ts,self.w)
# return self.CompleteVars(str_o)
# 
# def getActions(self):
#   if not self.Validate():
#   return ""
# str_a = self.getValue(4,self.ts,self.w)
# return self.CompleteVars(str_a)
# 
# def getReward(self):
#   if not self.Validate():
#   return ""
# return self.getValue(5,self.ts,self.w)
# 
# def getExtra(self):
#   if not self.Validate():
#   return ""
# return self.getValue(6,self.ts,self.w)
# 
# def isSpecial(self,maxOrMin):
#   if type(maxOrMin)!=type(""):
#   return False
# if maxOrMin=="UNSPEC" or maxOrMin=="NEGINF" or maxOrMin=="POSINF":
#   return True;
# else:
#   return False;
# 
# def getRange(self,str_input):
#   if not self.Validate():
#   return ""
# try:
#   str_input = str_input.replace("UNSPEC","'UNSPEC'")
# str_input = str_input.replace("NEGINF","'NEGINF'")
# str_input = str_input.replace("POSINF","'POSINF'")
# str_input = str_input.replace(" ",",")
# r = eval(str_input)
# if len(r)==2:
#   return [list(r)]
# 
# out = r[0]*([[r[1],r[2]]])
# return out
# 
# except:
#   self.last_error = "error ocurred while parsing a Range in "+str_input
# print "Warning: Malformed TaskSpec String: " +self.last_error
# print sys.exc_info()
# self.valid = False
# return ""
# 
# 
# 
# def getRewardRange(self):
#   if not self.Validate():
#   return ""
# str_reward = self.getReward()
# return self.getRange(str_reward)
# 
# def getVarInfoRange(self,i,ts,w):
#   self.Validate()
# a = ts.index(w[i])
# b = ts.index(w[i+1])+1
# return ts[a:b]
# 
# def GetVarValue(self,i,str_o):
#   if not self.Validate():
#   return ""
# str_r = self.getValue(i,str_o,self.v)
# str_r = str_r.replace(") (",")#(")
# # Ok I can parse it but this (there is no space or there is an extra space in ranges)
# # should be checked since this means that the taskspec is malformed
# str_r = str_r.replace("( ","(")
# str_r = str_r.replace(" )",")")
# str_r = str_r.replace(")(",")#(")
# 
# 
# parts = str_r.split("#")
# obs=[]
# for p in parts:
#   obs.extend(self.getRange(p))
# return obs
# 
# def getIntObservations(self):
#   if not self.Validate():
#   return ""
# return self.GetVarValue(0,self.getObservations())
# 
# def getDoubleObservations(self):
#   if not self.Validate():
#   return ""
# return self.GetVarValue(1,self.getObservations())
# 
# def getCharCountObservations(self):
#   if not self.Validate():
#   return ""
# str_o = self.getObservations()
# return int(self.getValue(2,str_o,self.v))
# 
# def getIntActions(self):
#   if not self.Validate():
#   return ""
# return self.GetVarValue(0,self.getActions())
# 
# def getDoubleActions(self):
#   if not self.Validate():
#   return ""
# return self.GetVarValue(1,self.getActions())
# 
# def getCharCountActions(self):
#   if not self.Validate():
#   return ""
# str_a = self.getActions()
# return int(self.getValue(2,str_a,self.v))
# 
