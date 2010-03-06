
#include "DistanceBased.h"

module DistanceBasedC 
{
    uses interface Boot;
    uses interface SplitControl as AMControl;
    uses interface Timer<TMilli> as Timer0;
    uses interface Timer<TMilli> as Timer1;
    uses interface Timer<TMilli> as Timer2;
    uses interface Timer<TMilli> as Timer3;
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
    uint8_t inibito=0;
    uint8_t inviati=0;
    uint8_t raggiunti=0;
    float dmin=0;
    float D=2;
    uint8_t seq=0;
    bool S2=FALSE;
    bool invioAck=FALSE;
    bool invioMsg=FALSE;
    message_t pkt;
    message_t pktToSend;
    message_t pktAck;
    MyMsg* toResend;
    MyMsg* msg;
    MyMsg* toAck;
    MyAck* ack;
    Elemento* lista=NULL;

    event void Boot.booted() 
    {
	dbg("default","Nodo %d avviato\n",TOS_NODE_ID);
	toResend = (MyMsg*) malloc(sizeof(MyMsg));
	msg = (MyMsg*) malloc(sizeof(MyMsg));
	toAck = (MyMsg*) malloc(sizeof(MyMsg));
	ack = (MyAck*) malloc(sizeof(MyAck));
	call AMControl.start();
    }

    event void AMControl.startDone(error_t err) 
    {
	if (err == SUCCESS) 
	{
	    if (TOS_NODE_ID==0)
	    {
		call Timer0.startPeriodicAt(1000,SEND_PERIOD);
	    }
	    //Viene avviato il timer per la stampa a video periodica delle prestazioni del protocollo (per comoditò di visualizzazione si visualizzano le prestazioni poco prima che il successivo messaggio viene inviato dal nodo 0)
	    call Timer2.startPeriodicAt(990-10*TOS_NODE_ID+SEND_PERIOD,SEND_PERIOD);
	}
	else 
	{
	    call AMControl.start();
	}
    }

    event void Timer0.fired() 
    {
	msg = (MyMsg*)(call Packet.getPayload(&pkt, sizeof(MyMsg)));
	if (msg == NULL) 
	    return;
	//Inviamo il messaggio msg con il nodo del mittente, il sequence number del messaggio e l'hop id (nel caso il pacchetto per raggiungere la destinazione deve passare per più nodi intermedi questo campo detiene l'ultimo nodo intermedio)
	msg->source_id = TOS_NODE_ID;
	seq++;
	msg->sequence_number = seq;
	msg->hop_id = TOS_NODE_ID;
	if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyMsg))==SUCCESS)
	{
	    //Si inbisce il nodo all'invio ulteriore di messaggi con stesso sequence number
	    inibito=msg->sequence_number;
	    dbg("default","\nIl nodo %d è inibito nell'inviare ulteriormente il messaggio %d\n",TOS_NODE_ID,inibito);
	    //Si incrementa la variabile del numero di messaggi inviati necessaria per il monitoraggio delle prestazioni del protocollo
	    inviati++;
	}
    }

    event void Timer1.fired() 
    {
	if (invioMsg==FALSE)
	{
	    //Se sono giunto qui non stò gia inviando un'altro messaggio e quindi posso inoltrare il messaggio precedentemente salvato (toResend)
	    memset(&pktToSend,0,sizeof(message_t));
	    msg = (MyMsg*)(call Packet.getPayload(&pktToSend, sizeof(MyMsg)));
	    dbg("default","\n\nE' scaduta l'attesa random del numero di slot, quindi inoltro il messaggio e attendo la fine della trasmissione!\n");
	    if (msg == NULL) 
		return;
	    //Copia le informazioni riguardanti il mittente del messaggio e il sequence number mentre modifica l'informazione sull'ultimo hop che ha inoltrato il messaggio
	    msg->source_id = toResend->source_id;
	    msg->sequence_number = toResend->sequence_number;
	    msg->hop_id = TOS_NODE_ID;
	    if (call AMSend.send(AM_BROADCAST_ADDR, &pktToSend, sizeof(MyMsg))==SUCCESS)
	    {
		//Se sono giunto qui allora il messaggio è stato preso in consegna per l'invio e quindi segnalo che c'è un messaggio in corso di invio, che la fase S2 è finita, inibisco il nodo per inviare ulteriormente il messaggio con stesso sequence number e incremento il numero di messaggi inviati
		invioMsg=TRUE;
		S2=FALSE;
		inibito=msg->sequence_number;
		dbg("default","Il nodo %d è inibito nell'inviare il messaggio %d\n",TOS_NODE_ID,inibito);
		inviati++;
	    }
	    else
	    {
		//Se sono giunto qui allora la richiesta di invio del messaggio non ha avuto successo, riavvio la procedura pià tardi
		dbg("default","Il nodo %d non è riuscito a inoltrare il messaggio, riprovo più tardi\n",TOS_NODE_ID);
		call Timer1.startOneShot(RESEND_PERIOD);
	    }
	}
	else
	{
	    //Se sono giunto qui allora è gia in fase di invio un'altro messaggio quindi riavvio la procedura pià tardi
	    dbg("default","E' scaduta l'attesa random del numero di slot, ma il nodo stà gia inviando un'altro messaggio aspetto ancora!\n");
	    call Timer1.startOneShot(RESEND_PERIOD);
	}
    }

    event void Timer2.fired() 
    {
	//Stampo a video periodicamente il numero di messaggi inviati dal nodo e il numero di nodi raggiunti per valutarne le performance e azzero poi le statistiche
	Elemento* tmp=lista;
	while (tmp!=NULL)
	{
	    lista=lista->next_elem;
	    free(tmp);
	    tmp=lista;
	}
	dbg("default","%s: Nodo %d ha inviato %d messaggi e ha raggiunto %d nodi (%f %)!\n",sim_time_string(),TOS_NODE_ID,inviati,raggiunti, (float)raggiunti*100/10);
	inviati=0;
	raggiunti=0;
    }

    event void Timer3.fired() 
    {
	//Mando il messaggio di ACK a chi le lo ha mandato per misurare le performance (nodi raggiunti)
	memset(&pktAck,0,sizeof(message_t));
	if (invioAck==FALSE)
	{
	    //Se sono giunto qui non stò gia inviando un'altro ack e quindi posso mandare l'ack specificando il nodo raggiunto e a chi è destinato l'ack
	    ack = (MyAck*)(call Packet.getPayload(&pktAck, sizeof(MyAck)));
	    if (ack == NULL) 
		return;
	    ack->mittente=toAck->hop_id;
	    ack->nodo_raggiunto = TOS_NODE_ID;
	    dbg("default","Ack in corso di invio per %d\n",ack->mittente);
	    if (call AMSend.send(AM_BROADCAST_ADDR, &pktAck, sizeof(MyAck))==SUCCESS)
		//Se sono giunto qui allora l'ack è stato preso in consegna per l'invio e quindi segnalo che c'è un ack in corso di invio
		invioAck=TRUE;
	    else
	    {
		//Se sono giunto qui allora la richiesta di invio dell'ack non ha avuto successo, riavvio la procedura pià tardi
		dbg("default","Il nodo %d non è riuscito a inoltrare l'ack per %d, riprovo più tardi\n",TOS_NODE_ID,ack->mittente);
		call Timer3.startOneShot(ACK_PERIOD);
	    }
	}
	else
	{
	    //Se sono giunto qui allora è gia in fase di invio un'altro ack quindi riavvio la procedura pià tardi
	    dbg("default","Il nodo stà gia inviando un'altro ack aspetto ancora!\n");
	    call Timer3.startOneShot(ACK_PERIOD);
	}
    }


    event void AMControl.stopDone(error_t err) 
    {
	Elemento* tmp=lista;
	free(toResend);
	free(msg);
	free(toAck);
	free(ack);
	while (tmp!=NULL)
	{
	    lista=lista->next_elem;
	    free(tmp);
	    tmp=lista;
	}
    }

    event void AMSend.sendDone(message_t* msg_snd, error_t err) 
    {
	//Viene stampato a video che è stato effettivamente inviato il messaggio/ack e si imposta la variabile di controllo in modo da permettere altri invii
	if (msg_snd==&pkt || msg_snd==&pktToSend)
	{
	    dbg("default","Nodo %d ha inviato un messaggio!\n",TOS_NODE_ID);
	    invioMsg=FALSE;
	}
	else if (msg_snd==&pktAck)
	{
	    dbg("default","Nodo %d ha inviato un ack!\n",TOS_NODE_ID);
	    invioAck=FALSE;
	}
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
	    //Si preleva il messaggio ricevuto e se ne tiene una copia per l'eventuale rinvio in S2 e per l'ack del messaggio ricevuto
	    msg = (MyMsg*)payload;
	    call Timer3.startOneShot(ACK_PERIOD);
	    if (msg->sequence_number>toResend->sequence_number)
		memcpy(toResend,msg,sizeof(MyMsg));
	    memcpy(toAck,msg,sizeof(MyMsg));
	    if (msg->sequence_number>last && msg->sequence_number>inibito)
	    {
		//Il messaggio ha sequence number diverso dall'ultimo ricevuto quindi sono nella fase S1
		uint8_t n_slot=0;
		//Calcolo la distanza utilizzando la formula : rssi=-(log10(distanza)*10*n+a)
		float distanza=pow(10,(float)(-a-rssi)/(10*n));
		//Inizializzo la variabile dmin al valore appena calcolato per la distanza (essendo la prima volta che ricevo questo messaggio) e mi salvo il suo sequence number come ultimo messaggio ricevuto
		dmin=distanza;
		last=msg->sequence_number;
		dbg("default","S1 -> Ho ricevuto il messaggio %d da %d e mittente %d con RSSI %d ottenendo una stima della distanza di %f\n",msg->sequence_number,msg->hop_id,msg->source_id,rssi,distanza);
		//Se la distanza minima non soddisfa le richieste passo alla fase S5 per non inoltrare il messaggio msg (non coprirei molta area in più)
		if (dmin<D)
		{
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltro il messaggio msg (impostando la variabile inibito)
		    call Timer1.stop();
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		    inibito=msg->sequence_number;
		    
		}
		else
		{
		    //Se sono arrivato qui allora il messaggio è arrivato per la prima volta da una sorgente che ha una distanza superiore a quella minima e quindi posso passare alla fase S2 (quindi inizializzo a TRUE la rispettiva variabile booleana) per tentare una ritrasmissione del messaggio msg
		    S2=TRUE;
		    //Ottengo un numero random di slot (di durata RESEND_PERIOD) per l'attesa di eventuali messaggi da altri nodi con distanza minore
		    n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    dbg("default","Attesa di %d slot\n",n_slot);
		    call Timer1.startOneShot(n_slot*RESEND_PERIOD);
		}
	    }
	    else if (msg->sequence_number==last && S2==TRUE && msg->sequence_number>inibito)
	    {
		//Ho ricevuto ancora lo stesso messaggio e sono nella fase S2 quindi interrompo l'attesa e passo alla fase S4
		float distanza=0;
		//Vengono prelevati i dati necessari per eventualmente riavviare il timer per l'attesa
		uint32_t periodo=call Timer1.getdt();
		uint32_t ultimo_fired=call Timer1.gett0();
		uint32_t now=call Timer1.getNow();
		call Timer1.stop();
		//Calcolo la nuova distanza di ricezione e verifico se è minore di quella minima ricevuta
		distanza=pow(10,(float)(-a-rssi)/(10*n));
		dbg("default","S2 -> Ho ricevuto il messaggio %d da %d e mittente %d con RSSI %d ottenendo una stima della distanza di %f\n",msg->sequence_number,msg->hop_id,msg->source_id,rssi,distanza);
		if (distanza<dmin)
		{
		    //Qui la distanza dal mittente è minore e quindi la aggiorno e verifico se rispetta ancora il limite definito (D)
		    dmin=distanza;
		    dbg("default","Aggiornata distanza: %f\n",dmin);
		}
		if (dmin<D)
		{
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltro il messaggio msg (impostando la variabile inibito)
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		    inibito=msg->sequence_number;
		}
		else
		{
		    //Ritorno in attesa su S2 utilizzando il tempo restante allo scattare del timer precedentemente salvato
		    dbg("default","Riprendo l'attesa S2 interrotta precedentemente\n");
		    call Timer1.startOneShot(ultimo_fired+periodo-now);
		}
	    }
	    else
	    {
		//Se sono giunto qui allora i casi precedenti non sono validi e quindi il messaggio non deve essere inviato
		dbg("default","Ho ricevuto il messaggio %d da %d ma non era conforme per l'invio\n",msg->sequence_number,msg->hop_id);
	    }
	}
	else if (len == sizeof(MyAck)) 
	{
	    //Se sono giunto qui allora il messaggio ricevuto è l'ack di un messaggio e quindi nel caso in cui è destinato a me (campo mittente dell'ack) aggiungo il nodo a quelli raggiunti da me
	    ack = (MyAck*)payload;
	    if (ack->mittente==TOS_NODE_ID)
	    {
		Elemento* tmp=lista;
		bool trovato=FALSE;
		dbg("default","Ricevuto ACK da %d\n",ack->nodo_raggiunto);
		//Verifico se il nodo è gia presente in lista, se non lo è lo aggiungo
		while(tmp!=NULL)
		{
		    if (tmp->nodo_raggiunto==ack->nodo_raggiunto)
			trovato=TRUE;
		    tmp=tmp->next_elem;
		}
		if (!trovato)
		{
		    Elemento* nuovo=(Elemento*) malloc(sizeof(Elemento));
		    nuovo->nodo_raggiunto=ack->nodo_raggiunto;
		    nuovo->next_elem=lista;
		    lista=nuovo;
		    raggiunti++;
		}
	    }
	}
	return msg_gen;
    }
}
