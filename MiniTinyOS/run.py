#!/usr/bin/python

N_MOTES = 10
DBG_CHANNELS = "default error"
SIM_TIME = 50
TOPO_FILE = "linkgain.out"
#NOISE_FILE = "/opt/tinyos-2.1.0/tos/lib/tossim/noise/TTX4-short.txt"
NOISE_FILE = "/opt/tinyos-2.1.0/tos/lib/tossim/noise/meyer-heavy.txt"

from TOSSIM import *
from tinyos.tossim.TossimApp import *
from random import *
import sys

t = Tossim([])
r = t.radio()

t.randomSeed(1)
for channel in DBG_CHANNELS.split():
    t.addChannel(channel, sys.stdout)

# AGGIUNTO LOGGING PERFORMANCE SU FILE Log.txt
log = open("log.txt", "w")
t.addChannel("log", log)

#add gain links
f = open(TOPO_FILE, "r")
lines = f.readlines()

for line in lines:
    s = line.split()
    if (len(s) > 0):
        if s[0] == "gain":
            r.add(int(s[1]), int(s[2]), float(s[3]))
        elif s[0] == "noise":
            r.setNoise(int(s[1]), float(s[2]), float(s[3]))
	
#add noise trace
noise = open(NOISE_FILE, "r")
lines = noise.readlines()
for line in lines:
    str = line.strip()
    if (str != ""):
        val = int(float(str))
        for i in range(0, N_MOTES):
            t.getNode(i).addNoiseTraceReading(val)


for i in range (0, N_MOTES):
    time=i * t.ticksPerSecond() / 100
    m=t.getNode(i)
    m.bootAtTime(time)
    m.createNoiseModel()
    print "Booting ", i, " at ~ ", time*1000/t.ticksPerSecond(), "ms"

time = t.time()
lastTime = -1
while (time + SIM_TIME * t.ticksPerSecond() > t.time()):
    timeTemp = int(t.time()/(t.ticksPerSecond()*10))
    if( timeTemp > lastTime ): #stampa un segnale ogni 10 secondi... per leggere meglio il log
        lastTime = timeTemp
        print "----------------------------------SIMULATION: ~", lastTime*10, " s ----------------------"
    t.runNextEvent()
print "----------------------------------END OF SIMULATION-------------------------------------"
