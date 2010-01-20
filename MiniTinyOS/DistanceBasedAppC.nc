/**
 * Application file for the DistanceBased application.  A counter is
 * incremented and a radio message is sent whenever a timer fires.
 * Whenever a radio message is received, the three least significant
 * bits of the counter in the message payload are displayed on the
 * LEDs.  Program two motes with this application.  As long as they
 * are both within range of each other, the LEDs on both will keep
 * changing.  If the LEDs on one (or both) of the nodes stops changing
 * and hold steady, then that node is no longer receiving any messages
 * from the other node.
 *
 * @author Prabal Dutta
 * @date   Feb 1, 2006
 */

#include "DistanceBased.h"

configuration DistanceBasedAppC {}
implementation 
{
    components MainC;
    components DistanceBasedC as App;
    components ActiveMessageC;
    components new TimerMilliC() as Timer0;
    components new TimerMilliC() as Timer1;
    components new AMSenderC(AM_DISTANCEBASED);
    components new AMReceiverC(AM_DISTANCEBASED);
    components RandomC;
    App.Boot -> MainC;
    App.AMControl -> ActiveMessageC;
    App.Timer0 -> Timer0;
    App.Timer1 -> Timer1;
    App.Packet -> AMSenderC;
    App.AMPacket -> AMSenderC;
    App.AMSend -> AMSenderC;
    App.Receive -> AMReceiverC;
    App.Random -> RandomC;
}
