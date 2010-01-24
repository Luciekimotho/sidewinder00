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
    uint8_t pos_x=0;
    uint8_t pos_y=0;
    uint8_t last=0;
    float dmin=0;
    float D=2.5;
    bool forwarded=FALSE;
    bool S2=FALSE;
    message_t pkt;
    MyMsg* toResend;
    /*message_t pktToSend;*/

    event void Boot.booted() 
    {
	dbg("default","Nodo %d avviato\n",TOS_NODE_ID);
	call AMControl.start();
    }

    event void AMControl.startDone(error_t err) 
    {
	if (err == SUCCESS) 
	{
	    //Calcolo delle posizioni x,y dei nodi TODO sarà poi da fare in base a quelle effettive
	    pos_x=(call Random.rand16()) % 10+1;
	    pos_y=(call Random.rand16()) % 10+1;
	    dbg("default","Posizione nodo %d: (%d,%d)\n",TOS_NODE_ID,pos_x,pos_y);
	    //TODO Solo il nodo 0 per ora invia i messaggi ma poi lo devono fare tutti
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
	    //Inviamo il messaggio msg con il nodo del mittente, il sequence number del messaggio e la posizione del nodo (necessaria per il calcolo della distanza tra mittente e destinatario)
	    msg->source_id = TOS_NODE_ID;
	    msg->sequence_number = 1;
	    msg->pos_x = pos_x;
	    msg->pos_y = pos_y;
	    if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyMsg))==SUCCESS)
	    //Si inizializza la variabile in modo da evitare di fare altri invii TODO Da levare poi perchè se ne fanno diversi
		forwarded=TRUE;
	}
    }

    event void Timer1.fired() 
    {
	/*//TODO Aggiungi copia buffer o blocco e farlo come Task
	MyMsg* msg = (MyMsg*)(call Packet.getPayload(&pktToSend, sizeof(MyMsg)));
	dbg("default","%s:\tInoltra msg e aspetta fino alla ine dell'invio\n",sim_time_string());
	if (msg == NULL) 
	    return;
	msg->source_id = toSend->source_id;
	msg->sequence_number = toSend->sequence_number;
	msg->pos_x = toSend->pos_x;
	msg->pos_y = toSend->pos_y;
	if (call AMSend.send(AM_BROADCAST_ADDR, &pktToSend, sizeof(MyMsg))==SUCCESS)
	    forwarded=TRUE;*/
    }

    event void AMControl.stopDone(error_t err) {}

    event void AMSend.sendDone(message_t* msg, error_t err) 
    {
	dbg("default","Nodo %d ha inviato un messaggio!\n",TOS_NODE_ID);
    }

    event message_t* Receive.receive(message_t* msg, void* payload, uint8_t len)
    {
	if (len == sizeof(MyMsg)) 
	{
	    //Si preleva il messaggio ricevuto e se ne tiene una copia per l'eventuale rinvio in S2
	    MyMsg* msg = (MyMsg*)payload;
	    toResend=msg;
	    if (msg->sequence_number>last)
	    {
		//Il messaggio ha sequence number diverso dall'ultimo ricevuto quindi sono nella fase S1
		uint8_t n_slot=0;
		//Calcolo la distanza tra mittente e destinatario
		float distanza=sqrt(((float)pos_x-(float)msg->pos_x)*((float)pos_x-(float)msg->pos_x)+((float)pos_y-(float)msg->pos_y)*((float)pos_y-(float)msg->pos_y));
		//Inizializzo la variabile dmin al valore appena calcolato per la distanza (essendo la prima volta che ricevo questo messaggio) e mi salvo il suo sequence number come ultimo messaggio ricevuto
		dmin=distanza;
		last=msg->sequence_number;
		dbg("default","La mia posizione è (%d,%d), ho ricevuto il messaggio da %d che ha posizione (%d,%d) (distanza %f)\n",pos_x,pos_y,msg->source_id,msg->pos_x,msg->pos_y,distanza);
		//Se la distanza minima non soddisfa le richieste passo alla fase S5 per non inoltrare il messaggio msg (non coprirei molta area in più)
		if (dmin<D)
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //TODO Interrompi trasmissione se in S2 e non inoltrare msg
		else
		{
		    //Se sono arrivato qui allora il messaggio è arrivato per la prima volta da una sorgente che ha una distanza superiore a quella minima e quindi posso passare alla fase S2 (quindi inizializzo a TRUE la rispettiva variabile booleana) per tentare una ritrasmissione del messaggio msg
		    S2=TRUE;
		    //Ottengo un numero random di slot (di durata TIMER_PERIOD) per l'attesa di eventuali messaggi da altri nodi con distanza minore
		    n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    dbg("default","Attesa di %d slot\n",n_slot);
		    call Timer1.startPeriodic(n_slot*TIMER_PERIOD);
		}
	    }
	    /*else if ((msg->sequence_number==last) && (S2==TRUE))
	    {
		float distanza=0;
		uint8_t n_slot=0;
		dbg("default","Se ero in S2 allora passo in S4\n");
		distanza=sqrt(((float)pos_x-(float)msg->pos_x)*((float)pos_x-(float)msg->pos_x)+((float)pos_y-(float)msg->pos_y)*((float)pos_y-(float)msg->pos_y));
		dbg("default","Mia osizione: (%d,%d)\tRicevuto da: %d\tCon posizione: (%d,%d)\tNuova distanza: %f\n",pos_x,pos_y,msg->source_id,msg->pos_x,msg->pos_y,distanza);
		if (distanza<dmin)
		{
		    dmin=distanza;
		    if (dmin<D)
			dbg("default","Non rispetta il limite, passo a S5\n");
		}
		else
		{
		    //Passo a S2
		    S2=TRUE;
		    n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    dbg("default","Attesa <- %d\n",n_slot);
		    call Timer1.startPeriodic(n_slot*TIMER_PERIOD);
		}
	    }*/
	}
	return msg;
    }
}
