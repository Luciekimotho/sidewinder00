/**
 * Implementation of the DistanceBased application.  A counter is
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

module DistanceBasedC 
{
    uses interface Boot;
    uses interface SplitControl as AMControl;
    uses interface Timer<TMilli> as Timer0;
    uses interface Timer<TMilli> as Timer1;
    uses interface Packet;
    uses interface AMPacket;
    uses interface AMSend;
    uses interface Receive;
    uses interface Random;
}

implementation 
{
    uint8_t pos_x;
    uint8_t pos_y;
    message_t pkt;
    bool forwarded=FALSE;
    uint8_t last=0;
    float dmin;
    float D=250;
    message_t pktToSend;
    MyMsg* toSend;

    event void Boot.booted() 
    {
	dbg("default","Nodo %d avviato\n", TOS_NODE_ID);
	call AMControl.start();
    }

    event void AMControl.startDone(error_t err) 
    {
	if (err == SUCCESS) 
	{
	    if (TOS_NODE_ID==0)
	    {
		call Timer0.startPeriodic(TIMER_PERIOD);
	    }
	}
	else 
	{
	    call AMControl.start();
	}
    }

    event void Timer0.fired() 
    {
	if (!forwarded)
	{
	    MyMsg* msg = (MyMsg*)(call Packet.getPayload(&pkt, sizeof(MyMsg)));
	    if (msg == NULL) 
		return;
	    pos_x=1;
	    pos_y=1;
	    msg->source_id = TOS_NODE_ID;
	    msg->sequence_number = 1;
	    msg->pos_x = pos_x;
	    msg->pos_y = pos_y;
	    if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyMsg))==SUCCESS)
		forwarded=TRUE;
	}
    }

    event void Timer1.fired() 
    {
	//TODO Aggiungi copia buffer o blocco e farlo come Task
	MyMsg* msg = (MyMsg*)(call Packet.getPayload(&pktToSend, sizeof(MyMsg)));
	dbg("default","%s:\tInoltra msg e aspetta fino alla ine dell'invio\n",sim_time_string());
	if (msg == NULL) 
	    return;
	msg->source_id = toSend->source_id;
	msg->sequence_number = toSend->sequence_number;
	msg->pos_x = toSend->pos_x;
	msg->pos_y = toSend->pos_y;
	if (call AMSend.send(AM_BROADCAST_ADDR, &pktToSend, sizeof(MyMsg))==SUCCESS)
	    forwarded=TRUE;
    }

    event void AMControl.stopDone(error_t err) {}

    event void AMSend.sendDone(message_t* msg, error_t err) 
    {
	dbg("default","Nodo %d ha inviato un messaggio!\n", TOS_NODE_ID);
    }

    event message_t* Receive.receive(message_t* msg, void* payload, uint8_t len)
    {
	if (len == sizeof(MyMsg)) 
	{
	    MyMsg* msg = (MyMsg*)payload;
	    toSend=msg;
	    if (msg->sequence_number>last)
	    {
		float distanza=0;
		uint8_t n_slot=0;
		pos_x=call Random.rand16();
		pos_y=call Random.rand16();
		distanza=sqrt(((float)pos_x-(float)msg->pos_x)*((float)pos_x-(float)msg->pos_x)+((float)pos_y-(float)msg->pos_x)*((float)pos_y-(float)msg->pos_x));
		dmin=distanza;
		dbg("default","%s:\tMittente <- %d\tPosizione <- (%d,%d)\tDistanza <- %f\n",sim_time_string(), msg->source_id,pos_x,pos_y, distanza);
		if (dmin<D)
		    dbg("default","Stoppa trasmissione in S2\n");
		else
		{
		    n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    dbg("default","Attesa <- %d\n",n_slot);
		    call Timer1.startPeriodic(n_slot*TIMER_PERIOD);
		}
	    }
	    else if (msg->sequence_number==last)
	    {
		dbg("default","Se ero in S2 allora passo in S4\n");
	    }
	}
	return msg;
    }
}
