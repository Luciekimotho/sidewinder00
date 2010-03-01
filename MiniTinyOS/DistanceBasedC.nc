
#include "DistanceBased.h"

module DistanceBasedC 
{
    uses interface Boot;
    uses interface SplitControl as AMControl;
    uses interface Timer<TMilli> as Timer0;
    uses interface Timer<TMilli> as Timer1;
    uses interface Timer<TMilli> as Timer2;
    uses interface Packet;
    uses interface AMPacket;
    uses interface AMSend;
    uses interface Receive;
    uses interface Random;
    uses interface TossimPacket;
}

implementation 
{
    uint8_t last=0;
    uint8_t inviati=0;
    uint8_t raggiunti=0;
    float dmin=0;
    float D=2.5;
    bool forwarded=FALSE;
    bool S2=FALSE;
    message_t pkt;
    MyMsg* toResend;
    message_t pktToSend;

    event void Boot.booted() 
    {
	dbg("default","Nodo %d avviato\n",TOS_NODE_ID);
	call AMControl.start();
    }

    event void AMControl.startDone(error_t err) 
    {
	if (err == SUCCESS) 
	{
	    //TODO Solo il nodo 0 per ora invia i messaggi ma poi lo devono fare tutti
	    if (TOS_NODE_ID==0)
	    {
		call Timer0.startPeriodic(TIMER_PERIOD);
	    }
	    //Viene avviato il timer per la stampa a video periodica delle prestazioni del protocollo
	    call Timer2.startPeriodic(PERFORMANCE_PERIOD);
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
	    msg->hop_id = TOS_NODE_ID;
	    if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyMsg))==SUCCESS)
	    {
		//Si inizializza la variabile in modo da evitare di fare altri invii TODO Da levare poi perchè se ne fanno diversi
		forwarded=TRUE;
		inviati++;
	    }
	}
    }

    event void Timer1.fired() 
    {
	//TODO Aggiungi copia buffer o blocco e farlo come Task
	MyMsg* msg = (MyMsg*)(call Packet.getPayload(&pktToSend, sizeof(MyMsg)));
	dbg("default","E' scaduta l'attesa random del numero di slot, quindi inoltro il messaggio e attendo la fine della trasmissione!\n");
	if (msg == NULL) 
	    return;
	//Copia le informazioni riguardanti il mittente del messaggio e il sequence number mentre modifica l'informazione sull'ultimo hop che ha inoltrato il messaggio e la sua posizione
	msg->source_id = toResend->source_id;
	msg->sequence_number = toResend->sequence_number;
	msg->hop_id = TOS_NODE_ID;
	if (call AMSend.send(AM_BROADCAST_ADDR, &pktToSend, sizeof(MyMsg))==SUCCESS)
	{
	    S2=FALSE;
	    inviati++;
	}
    }

    event void Timer2.fired() 
    {
	//Stampo a video periodicamente il numero di messaggi inviati e ricevuti dal nodo per valutarne le performance
	dbg("default","Nodo %d ha inviato %d messaggi e ha raggiunto %d nodi (%f %)!\n",TOS_NODE_ID,inviati,raggiunti, (float)raggiunti*100/10);
	inviati=0;
	raggiunti=0;
    }

    event void AMControl.stopDone(error_t err) {}

    event void AMSend.sendDone(message_t* msg, error_t err) 
    {
	dbg("default","Nodo %d ha inviato un messaggio!\n",TOS_NODE_ID);
    }

    event message_t* Receive.receive(message_t* msg_gen, void* payload, uint8_t len)
    {
	if (len == sizeof(MyMsg)) 
	{
	    //Definisco "a" con il valore per l'RSSI a distanza di 1 metro (preso dal file TopoConfig)
	    uint8_t a=55.4;
	    //Definisco "n" come la costante di propagazione del canale (preso dal file TopoConfig)
	    uint8_t n=4.7;
	    //Ottengo il valore dell'RSSI dal messaggio ricevuto
	    int8_t rssi=call TossimPacket.strength(msg_gen);
	    //Si preleva il messaggio ricevuto e se ne tiene una copia per l'eventuale rinvio in S2
	    MyMsg* msg = (MyMsg*)payload;
	    //Mando il messaggio di ACK a chi le lo ha mandato per misurare le performance
	    MyAck* ack = (MyAck*)(call Packet.getPayload(&pkt, sizeof(MyAck)));
	    if (ack == NULL) 
		return msg_gen;
	    ack->mittente=msg->hop_id;
	    //TODO VEDI SE TENERLO ack->sequence_number = msg->sequence_number;
	    ack->nodo_raggiunto = TOS_NODE_ID;
	    call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyAck))==SUCCESS;
	    toResend=msg;
	    if (msg->sequence_number>last)
	    {
		//Il messaggio ha sequence number diverso dall'ultimo ricevuto quindi sono nella fase S1
		uint8_t n_slot=0;
		//Calcolo la distanza utilizzando la formula : rssi=-(log10(distanza)*10*n+a)
		float distanza=pow(10,(float)(-a-rssi)/(10*n));
		//Inizializzo la variabile dmin al valore appena calcolato per la distanza (essendo la prima volta che ricevo questo messaggio) e mi salvo il suo sequence number come ultimo messaggio ricevuto
		dmin=distanza;
		last=msg->sequence_number;
		dbg("default","S1 -> Ho ricevuto il messaggio da %d e mittente %d con RSSI %d ottenendo una stima della distanza di %f\n",msg->hop_id,msg->source_id,rssi,distanza);
		//Se la distanza minima non soddisfa le richieste passo alla fase S5 per non inoltrare il messaggio msg (non coprirei molta area in più)
		if (dmin<D)
		{
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltraro il messaggio msg
		    call Timer1.stop();
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		    
		}
		else
		{
		    //Se sono arrivato qui allora il messaggio è arrivato per la prima volta da una sorgente che ha una distanza superiore a quella minima e quindi posso passare alla fase S2 (quindi inizializzo a TRUE la rispettiva variabile booleana) per tentare una ritrasmissione del messaggio msg
		    S2=TRUE;
		    //Ottengo un numero random di slot (di durata TIMER_PERIOD) per l'attesa di eventuali messaggi da altri nodi con distanza minore
		    n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    dbg("default","Attesa di %d slot\n",n_slot);
		    call Timer1.startOneShot(n_slot*TIMER_PERIOD);
		}
	    }
	    else if ((msg->sequence_number==last) && (S2==TRUE))
	    {
		//Ho ricevuto ancora lo stesso messaggio e sono nella fase S2 quindi interrompo l'attesa e passo alla fase S4
		float distanza=0;
		uint8_t n_slot=0;
		call Timer1.stop();
		//Calcolo la nuova distanza di ricezione e verifico se è minore di quella minima ricevuta
		distanza=pow(10,(float)(-a-rssi)/(10*n));
		dbg("default","S2 -> Ho ricevuto il messaggio da %d e mittente %d con RSSI %d ottenendo una stima della distanza di %f\n",msg->hop_id,msg->source_id,rssi,distanza);
		if (distanza<dmin)
		{
		    //Qui la distanza dal mittente è minore e quindi la aggiorno e verifico se rispetta ancora il limite definito (D)
		    dmin=distanza;
		    dbg("default","Aggiornata distanza: %f\n",dmin);
		    if (dmin<D)
			dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltraro il messaggio msg
		    call Timer1.stop();
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		}
		else
		{
		    //Ritorno in attesa su S2
		    call Timer1.startOneShot(n_slot*TIMER_PERIOD);
		}
	    }
	}
	else if (len == sizeof(MyAck)) 
	{
	    MyAck* ack = (MyAck*)payload;
	    if (ack->mittente==TOS_NODE_ID)
	    {
		dbg("default","Ricevuto ACK da %d\n",ack->nodo_raggiunto);
		raggiunti++;
	    }
	}
	return msg_gen;
    }
}
