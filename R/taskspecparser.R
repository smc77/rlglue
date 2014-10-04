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

TaskSpec <- R6Class("TaskSpec",
                   public = list(
                     w = c("VERSION","PROBLEMTYPE","DISCOUNTFACTOR", "OBSERVATIONS","ACTIONS","REWARDS","EXTRA"), 
                     v=c("INTS","DOUBLES","CHARCOUNT"), 
                     expected_version = "RL-Glue-3.0",
                     valid = TRUE,
                     last_error = "",
                     ts = "",
                     init = function(ts) {
                       # Replace white spaces
                       while(any(grep("  ", ts))) ts <- gsub("  ", " ", ts)
                       self$ts <- strsplit(ts, " ")[[1]]                       
                       if(self$expected_version != self$getVersion()){
                         stop(paste("Expected version does not match:", self$expected_version, "-", self$getVersion()))
                         self$valid = FALSE
                       }
                     },
                     getValue = function(i=NA, name=NA) {
                       if(is.na(i) && !is.na(name))
                         i <- which(w == name)
                       if(!any(i)) stop(paste("Did not specify getValue properly:", i, name))
                       if(i > length(w)) stop(paste("There are no values beyond i:", i))
                       start = which(ts == w[i]) + 1
                       end = if(length(w) < i + 1) length(ts) else which(ts == w[i + 1]) - 1                       
                       return(paste(ts[start:end], collapse=" "))
                     },
                     Validate = function() {
                       if(!self$valid) {
                         print(paste("Warning: task specification appears to be invalid.", last_error))
                         return(FALSE)
                       }
                       return(TRUE)
                     },
                     getVersion = function() {
                       if(!self$Validate()) return("")
                       return(self$getValue(name="VERSION"))
                     },
                     getProblemType = function() {
                       if(!self$Validate()) return("")
                       return(self$getValue(name="PROBLEMTYPE"))
                     },
                     getDiscountFactor = function() {
                       if(!self$Validate()) return("")
                       return(self$getValue(name="DISCOUNTFACTOR"))
                     },
                     getReward = function() {
                       if(!self$Validate()) return("")
                       return(self$getValue(name="REWARDS"))
                     },
                     getExtra = function() {
                       if(!self$Validate()) return("")
                       return(self$getValue(name="EXTRA"))
                     },
                     CompleteVars = function(str_in) {
                       if(!self$Validate()) return("")
                       if(!any(grepl(self$v[1], str_in))) {
                         str_in = paste(self$v[1], " (0 0 0) ", str_in, sep="")
                       }
                       if(!any(grepl(self$v[2], str_in))) {
                         str_in = paste(str_in, " ", self$v[2], " (0, 0, 0) ", sep="")
                       }
                       if(!any(grepl(self$v[3], str_in))) {
                         str_in = paste(str_in, " ", self$v[3], " 0 ", sep="")
                       }
                       return(str_in)
                     },
                     getObservations = function() {
                       if(!self$Validate()) return("")
                       return(self$CompleteVars(self$getValue(name="OBSERVATIONS")))
                     },
                     getActions = function() {
                       if(!self$Validate()) return("")
                       return(self$CompleteVars(self$getValue(name="ACTIONS")))
                     },
                     isSpecial = function(maxOrMin) {
                       if(!is.character(maxOrMin)) return(FALSE)
                       if(maxOrMin=="UNSPEC" || maxOrMin=="NEGINF" || maxOrMin=="POSINF") return(TRUE)
                       return(FALSE)
                     },
                     getRange = function(str_input) {
                       if(!self$Validate()) return("")
                       result <- try({
                         str_input = gsub("UNSPEC","'UNSPEC'", str_input)
                         str_input = gsub("NEGINF","'NEGINF'", str_input)
                         str_input = gsub("POSINF","'POSINF'", str_input)
                         str_input = gsub(" ",",", str_input)
                         str_input = gsub("\\(","c(", str_input)
                         
                         r = eval(parse(text=str_input))
                         if(length(r) == 2) return(r)
                         out = rep(c(r[2], r[3]), r[1])
                         return(out)                         
                       })
                       if(class(result) == "try-error") {
                         self$last_error = paste("error ocurred while parsing a Range in", str_input)
                         self$valid = FALSE
                         return("")
                       }
                     },
                     getRewardRange = function() {
                       if(!self$Validate()) return("")
                       str_reward = self$getReward()
                       return(self$getRange(str_reward))
                     },                     
                     getVarInfoRange = function(i, ts, w) {
                       self$Validate()
#                        a = ts[w[i]]
#                        b = ts[w[i+1]]+1
#                        return(substr(ts, a:b]) 
                       return(self$getValue(name=w[i]))
                     }
                   )
)

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


test.TaskSpec <- function() {
  errors <- c()
  ts <- TaskSpec$new()
  ts$init(tsp.example)
  if(ts$getVersion() != "RL-Glue-3.0") errors <- c(errors, "Could not get version.")
  if(ts$getProblemType() != "episodic") errors <- c(errors, "Could not get problem type.")
  if(length(errors)) {
    print(paste("There were errors: ", errors, collapse=";"))
    return(FALSE)
  } else {
    print("Test passed.")
  }  
  return(TRUE)
}