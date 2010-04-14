
#include "DistanceBased.h"

configuration DistanceBasedAppC 
{
}

implementation 
{
    components MainC;
    components DistanceBasedC as App;
    components ActiveMessageC;
    components new TimerMilliC() as Timer0;
    components new TimerMilliC() as Timer1;
    components new TimerMilliC() as Timer2;
    components new AMSenderC(AM_DISTANCEBASED);
    components new AMReceiverC(AM_DISTANCEBASED);
    components RandomC;
    components TossimActiveMessageC;
    App.Boot -> MainC;
    App.AMControl -> ActiveMessageC;
    App.Timer0 -> Timer0;
    App.Timer1 -> Timer1;
    App.Timer2 -> Timer2;
    App.Packet -> AMSenderC;
    App.AMPacket -> AMSenderC;
    App.AMSend -> AMSenderC;
    App.Receive -> AMReceiverC;
    App.Random -> RandomC;
    App.TossimPacket -> TossimActiveMessageC;
}
