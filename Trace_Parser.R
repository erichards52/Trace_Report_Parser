# Trace report investigation
# Read in files

library(ggplot2)
library(kableExtra)
library(lubridate)
library(kableExtra)
library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)


filenames_dirs <- list.dirs("/Users/edwardrichards/Documents/All_trace/",full.names=TRUE)
filecounter = 0

for (file in filenames_dirs) {
filenames <- list.files(file, pattern="*.txt", full.names=TRUE)
#ldf <- lapply(filenames, read.delim)
ldf <- map2(map(filenames, read.delim), filenames, cbind)
#ldf <- lapply(tester, read.delim)

colnames <- c("task_id","hash","name","process","status","exit",
      "container","cpus","time","disk","memory","rss","X.cpu","X.mem","vmem","peak_rss",
      "peak_vmem","submit","start","complete","duration","realtime","rchar","wchar","read_bytes","write_bytes",
      "workdir","flowcell_id") 
for (i in seq_along(ldf)){
colnames(ldf[[i]]) <- colnames
}


# Workdir
result = lapply(ldf, "[", , "workdir")

# Put the name of each process into a uniquely named variable and do the following below for each (between brackets)
ldfProcess <- lapply(ldf, subset, grepl("PROCESS", process))
processList <- lapply(ldfProcess, function(x){
x[['duration']]
})

realtimeList <- lapply(ldfProcess, function(x){
x[['realtime']]
})

cpuList <- lapply(ldfProcess, function(x){
x[['X.cpu']]
})

memList <- lapply(ldfProcess, function(x){
x[['vmem']]
})

rcharList <- lapply(ldfProcess, function(x){
x[['rchar']]
})

wcharList <- lapply(ldfProcess, function(x){
x[['wchar']]
})

# Duration parser

durationVec <- unlist(processList, use.names = F)
for (i in 1:length(durationVec)){
if (grepl('d',durationVec[i]) && grepl('h',durationVec[i]) && grepl('m',durationVec[i]) && grepl('s',durationVec[i]))  {
intDays=str_match(durationVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
durationVec[i] <- gsub("[0-9]d ","", durationVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(durationVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
durationVec[i] <- gsub(paste(intHours, "h ", sep=""),":", paste(totHours, durationVec[i], sep=""))
durationVec[i] <- gsub("m ",":", durationVec[i])
durationVec[i] <- gsub("s","", durationVec[i])
durationVec[i] <- gsub(' :00 ',':00',durationVec[i])
} else  if (grepl('d',durationVec[i]) && grepl('h',durationVec[i]) && grepl('s',durationVec[i]))  {
intDays=str_match(durationVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
durationVec[i] <- gsub("[0-9]d ","", durationVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(durationVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
durationVec[i] <- gsub(paste(intHours, "h ", sep=""),":00:", paste(totHours, durationVec[i], sep=""))
durationVec[i] <- gsub("s","", durationVec[i])
durationVec[i] <- gsub(' :00 ',':00',durationVec[i])
} else if (grepl('d',durationVec[i]) && grepl('h',durationVec[i]) && grepl('m',durationVec[i]))  {
intDays=str_match(durationVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
durationVec[i] <- gsub("[0-9]d ","", durationVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(durationVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
durationVec[i] <- gsub(paste(intHours, "h ", sep=""),":", paste(totHours, durationVec[i], sep=""))
durationVec[i] <- gsub("m",":00", durationVec[i])
} else if (grepl('d',durationVec[i]) && grepl('m',durationVec[i]) && grepl('s',durationVec[i]))  {
intDays=str_match(durationVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
durationVec[i] <- gsub("[0-9]d ","", durationVec[i])
intTotDays=as.numeric(intDays)*24
durationVec[i] <- paste(c(intTotDays,":", durationVec[i]),collapse = "")
durationVec[i] <- gsub("m ",":", durationVec[i])
durationVec[i] <- gsub("s","", durationVec[i])
durationVec[i] <- gsub(' :00 ',':00',durationVec[i])
} else if (grepl('h',durationVec[i]) && grepl('m',durationVec[i]) && grepl('s',durationVec[i]))  {
durationVec[i] <- gsub("h ",":", durationVec[i])
durationVec[i] <- gsub("m ",":", durationVec[i])
durationVec[i] <- gsub("s","", durationVec[i])
durationVec[i] <- gsub(' :00 ',':00',durationVec[i])
} else if (grepl('h', durationVec[i]) && grepl('s',durationVec[i])) {
durationVec[i] <- gsub("h ",':00:', durationVec[i])
durationVec[i] <- gsub("s","", durationVec[i])
durationVec[i] <- gsub(' :00 ',':00',durationVec[i])
} else if (grepl('m', durationVec[i]) && grepl('s', durationVec[i])) {
durationVec[i] <- paste(c("00:", durationVec[i]),collapse = "")
durationVec[i] <- gsub('m ',':',durationVec[i])
durationVec[i] <- gsub('s','',durationVec[i])
} else if (grepl('h', durationVec[i]) && grepl('m',durationVec[i])) {
durationVec[i] <- gsub('m',':00',durationVec[i])
durationVec[i] <- gsub('h ',':',durationVec[i])
} else if (grepl('h', durationVec[i])) {
durationVec[i] <- gsub('h',':00:00',durationVec[i])
} else if (grepl('m',durationVec[i])) {
durationVec[i] <- paste(c("00:", durationVec[i]),collapse = "")
durationVec[i] <- gsub('m',':00',durationVec[i])
} else if (grepl('-',durationVec[i])) { 
durationVec[i] <- gsub('-',NA,durationVec[i])
} else if (grepl('s',durationVec[i])) { 
durationVec[i] <- gsub('s','',durationVec[i])
durationVec[i] <- paste('00:00:', durationVec[i])
durationVec[i] <- gsub(' ','',durationVec[i])
}
}
durationVec <- na.omit(durationVec)
durationVec <- lubridate::hms(durationVec)
durationVec <- na.omit(durationVec)
AvgDuration <- seconds_to_period(mean(period_to_seconds(durationVec)))
print(AvgDuration)

# realtime parser
realtimeVec <- unlist(realtimeList, use.names = F)
for (i in 1:length(realtimeVec)){
if (grepl('d',realtimeVec[i]) && grepl('h',realtimeVec[i]) && grepl('m',realtimeVec[i]) && grepl('s',realtimeVec[i]))  {
intDays=str_match(realtimeVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
realtimeVec[i] <- gsub("[0-9]d ","", realtimeVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(realtimeVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
realtimeVec[i] <- gsub(paste(intHours, "h ", sep=""),":", paste(totHours, realtimeVec[i], sep=""))
realtimeVec[i] <- gsub("m ",":", realtimeVec[i])
realtimeVec[i] <- gsub("s","", realtimeVec[i])
realtimeVec[i] <- gsub(' :00 ',':00',realtimeVec[i])
} else  if (grepl('d',realtimeVec[i]) && grepl('h',realtimeVec[i]) && grepl('s',realtimeVec[i]))  {
intDays=str_match(realtimeVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
realtimeVec[i] <- gsub("[0-9]d ","", realtimeVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(realtimeVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
realtimeVec[i] <- gsub(paste(intHours, "h ", sep=""),":00:", paste(totHours, realtimeVec[i], sep=""))
realtimeVec[i] <- gsub("s","", realtimeVec[i])
realtimeVec[i] <- gsub(' :00 ',':00',realtimeVec[i])
} else if (grepl('d',realtimeVec[i]) && grepl('h',realtimeVec[i]) && grepl('m',realtimeVec[i]))  {
intDays=str_match(realtimeVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
realtimeVec[i] <- gsub("[0-9]d ","", realtimeVec[i])
intTotDays=as.numeric(intDays)*24
intHours=str_match(realtimeVec[i], '([^:]+)(?:h[^h]+){1}$')[,2]
intHours=as.numeric(intHours)
totHours=intTotDays+intHours
realtimeVec[i] <- gsub(paste(intHours, "h ", sep=""),":", paste(totHours, realtimeVec[i], sep=""))
realtimeVec[i] <- gsub("m",":00", realtimeVec[i])
} else if (grepl('d',realtimeVec[i]) && grepl('m',realtimeVec[i]) && grepl('s',realtimeVec[i]))  {
intDays=str_match(realtimeVec[i], '([^d]+)(?:d[^d]+){1}$')[,2]
intDays=as.numeric(intDays)
realtimeVec[i] <- gsub("[0-9]d ","", realtimeVec[i])
intTotDays=as.numeric(intDays)*24
realtimeVec[i] <- paste(c(intTotDays,":", realtimeVec[i]),collapse = "")
realtimeVec[i] <- gsub("m ",":", realtimeVec[i])
realtimeVec[i] <- gsub("s","", realtimeVec[i])
realtimeVec[i] <- gsub(' :00 ',':00',realtimeVec[i])
} else if (grepl('h',realtimeVec[i]) && grepl('m',realtimeVec[i]) && grepl('s',realtimeVec[i]))  {
realtimeVec[i] <- gsub("h ",":", realtimeVec[i])
realtimeVec[i] <- gsub("m ",":", realtimeVec[i])
realtimeVec[i] <- gsub("s","", realtimeVec[i])
realtimeVec[i] <- gsub(' :00 ',':00',realtimeVec[i])
} else if (grepl('h', realtimeVec[i]) && grepl('s',realtimeVec[i])) {
realtimeVec[i] <- gsub("h ",':00:', realtimeVec[i])
realtimeVec[i] <- gsub("s","", realtimeVec[i])
realtimeVec[i] <- gsub(' :00 ',':00',realtimeVec[i])
} else if (grepl('m', realtimeVec[i]) && grepl('s', realtimeVec[i])) {
realtimeVec[i] <- paste(c("00:", realtimeVec[i]),collapse = "")
realtimeVec[i] <- gsub('m ',':',realtimeVec[i])
realtimeVec[i] <- gsub('s','',realtimeVec[i])
} else if (grepl('h', realtimeVec[i]) && grepl('m',realtimeVec[i])) {
realtimeVec[i] <- gsub('m',':00',realtimeVec[i])
realtimeVec[i] <- gsub('h ',':',realtimeVec[i])
} else if (grepl('h', realtimeVec[i])) {
realtimeVec[i] <- gsub('h',':00:00',realtimeVec[i])
} else if (grepl('m',realtimeVec[i])) {
realtimeVec[i] <- paste(c("00:", realtimeVec[i]),collapse = "")
realtimeVec[i] <- gsub('m',':00',realtimeVec[i])
} else if (grepl('-',realtimeVec[i])) { 
realtimeVec[i] <- gsub('-',NA,realtimeVec[i])
} else if (grepl('s',realtimeVec[i])) { 
realtimeVec[i] <- gsub('s','',realtimeVec[i])
realtimeVec[i] <- paste('00:00:', realtimeVec[i])
realtimeVec[i] <- gsub(' ','',realtimeVec[i])
}
}
realtimeVec <- na.omit(realtimeVec)
realtimeVec <- lubridate::hms(realtimeVec)
realtimeVec <- na.omit(realtimeVec)
AvgRealtime <- seconds_to_period(mean(period_to_seconds(realtimeVec)))

print(AvgRealtime)
# cpu parser
cpuVec <- unlist(cpuList, use.names = F)
cpuVec <- gsub('%','',cpuVec)
cpuVec <- gsub('-','',cpuVec)
cpuVec <- as.numeric(cpuVec)
cpuVec <- na.omit(cpuVec)
cpuAvg <- mean(cpuVec)

print(cpuAvg)
  
# Memory parser
memVec <- unlist(memList, use.names = F)
for (i in 1:length(memVec)) {
if (grepl('GB',memVec[i])) {
memVec[i] <- gsub('GB','',memVec[i])
} else if (grepl('MB',memVec[i])) {
memVec[i] <- gsub('MB','',memVec[i])
memVec[i] <- as.integer(memVec[i])
memVec[i] <-paste(c("0.", memVec[i]),collapse = "")
}
}

memVec <- as.numeric(memVec)
memVec <- na.omit(memVec)
memAvg <- mean(memVec)
  
print(memAvg)

# Read characters parser
rcharVec <- unlist(rcharList, use.names = F)
for (i in 1:length(rcharVec)) {
if (grepl('GB',rcharVec[i])) {
rcharVec[i] <- gsub('GB','',rcharVec[i])
} else if (grepl('MB',rcharVec[i])) {
rcharVec[i] <- gsub('MB','',rcharVec[i])
rcharVec[i] <- as.integer(rcharVec[i])

if (nchar(rcharVec[i])==1) {
rcharVec[i] <- paste(c("0.00", rcharVec[i]),collapse = "")
} else if (nchar(rcharVec[i])==2) {
rcharVec[i] <-paste(c("0.0", rcharVec[i]),collapse = "")
} else if (nchar(rcharVec[i])==3) {
rcharVec[i] <-paste(c("0.", rcharVec[i]),collapse = "")
}
} else if (grepl('KB',rcharVec[i])) {
rcharVec[i] <- gsub('KB','',rcharVec[i])
rcharVec[i] <- gsub("\\.","",rcharVec[i])
rcharVec[i] <-paste(c("0.000", rcharVec[i]),collapse = "")
} else if (grepl('B',rcharVec[i])) {
rcharVec[i] <- gsub('B','',rcharVec[i])
rcharVec[i] <- gsub("\\.","",rcharVec[i])
rcharVec[i] <-paste(c("0.0000", rcharVec[i]),collapse = "")
}
}
rcharVec <- as.numeric(rcharVec)
rcharVec <- na.omit(rcharVec)
rcharAvg <- mean(rcharVec)
  
print(rcharAvg)

# Write characters parser
wcharVec <- unlist(wcharList, use.names = F)
for (i in 1:length(wcharVec)) {
if (grepl('GB',wcharVec[i])) {
wcharVec[i] <- gsub('GB','',wcharVec[i])
} else if (grepl('MB',wcharVec[i])) {
wcharVec[i] <- gsub('MB','',wcharVec[i])
wcharVec[i] <- as.integer(wcharVec[i])

if (nchar(wcharVec[i])==1) {
wcharVec[i] <- paste(c("0.00", wcharVec[i]),collapse = "")
} else if (nchar(wcharVec[i])==2) {
wcharVec[i] <-paste(c("0.0", wcharVec[i]),collapse = "")
} else if (nchar(wcharVec[i])==3) {
wcharVec[i] <-paste(c("0.", wcharVec[i]),collapse = "")
}
} else if (grepl('KB',wcharVec[i])) {
wcharVec[i] <- gsub('KB','',wcharVec[i])
wcharVec[i] <- gsub("\\.","",wcharVec[i])
wcharVec[i] <-paste(c("0.000", wcharVec[i]),collapse = "")
} else if (grepl('B',wcharVec[i])) {
wcharVec[i] <- gsub('B','',wcharVec[i])
wcharVec[i] <- gsub("\\.","",wcharVec[i])
wcharVec[i] <-paste(c("0.0000", wcharVec[i]),collapse = "")
}
}
wcharVec <- as.numeric(wcharVec)
format(round(wcharVec, 2), nsmall = 2)
wcharVec <- na.omit(wcharVec)
wcharAvg <- mean(wcharVec)

print(wcharAvg)

