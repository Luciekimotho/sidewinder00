#include "DistanceBased.h"

#if 0
#define n_slot 5
#else
#define n_slot ((uint8_t)(call Random.rand16()))%20+10
#endif

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
    uint8_t seq=0;
    float dmin=0;
    float D=2;    
    Inviato* inviati=NULL;
    Inoltrato* inoltrati=NULL;
    Ricevuto* ricevuti=NULL;
    Inibito* inibiti=NULL;
    DaInoltrare* daInoltrare=NULL;
    SeqRicevuta* seqRicevute=NULL;
    bool invioMsg=FALSE;
    message_t pkt;
    message_t pktToSend;
    MyMsg *msg;

    event void Boot.booted() 
    {
	dbg("default","Nodo %d avviato\n",TOS_NODE_ID);
	msg = (MyMsg*) malloc(sizeof(MyMsg));
	call AMControl.start();
    }

    event void AMControl.startDone(error_t err) 
    {
	if (err == SUCCESS) 
	{
	    call Timer0.startOneShot((((uint8_t)(call Random.rand16()))%20+10)*SEND_PERIOD);
	    //Viene avviato il timer per la stampa a video periodica delle prestazioni del protocollo (per comoditò di visualizzazione si visualizzano le prestazioni poco prima che il successivo messaggio viene inviato dal nodo 0)
	    call Timer2.startPeriodic(PERF_PERIOD);
	}
	else 
	{
	    call AMControl.start();
	}
    }

    event void Timer0.fired() 
    {
	//TODO Da rimuovere
	if (seq<3)
	{
	    //Verifico se il messaggio da inviare non è gia inibito
	    Inibito* tmp=inibiti;
	    bool inibito=FALSE;
	    seq++;	    
	    while(tmp!=NULL)
	    {
		if (tmp->sequence_number==seq)
		    inibito=TRUE;
		tmp=tmp->next;
	    }
	    if (!inibito)
	    {
		dbg("default","\nIl nodo %d è pronto ad inviare un nuovo messaggio\n",TOS_NODE_ID);
		msg = (MyMsg*)(call Packet.getPayload(&pkt, sizeof(MyMsg)));
		if (msg == NULL) 
		    return;
		//Inviamo il messaggio msg con il nodo del mittente, il sequence number del messaggio e l'hop id (nel caso il pacchetto per raggiungere la destinazione deve passare per più nodi intermedi questo campo detiene l'ultimo nodo intermedio)
		msg->source_id = TOS_NODE_ID;
		msg->sequence_number = seq;
		msg->hop_id = TOS_NODE_ID;
		if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(MyMsg))==SUCCESS)
		{
		    Inviato* nuovoInviato=NULL;
		    //Si inbisce il nodo all'invio ulteriore di messaggi con stesso sequence number
		    Inibito* nuovoInibito=(Inibito*) malloc(sizeof(Inibito));
		    nuovoInibito->sequence_number=seq;
		    nuovoInibito->next=inibiti;
		    inibiti=nuovoInibito;
		    dbg("default","Il nodo %d è inibito nell'inviare ulteriormente il messaggio %d\n",TOS_NODE_ID,seq);
		    //Si incrementa la variabile del numero di messaggi inviati necessaria per il monitoraggio delle prestazioni del protocollo
		    nuovoInviato=(Inviato*) malloc(sizeof(Inviato));
		    nuovoInviato->sequence_number=msg->sequence_number;
		    nuovoInviato->next=NULL;
		    if (inviati==NULL)
			inviati=nuovoInviato;
		    else
		    {
			Inviato* tmp4=inviati;
			while (tmp4->next!=NULL)
			    tmp4=tmp4->next;
			tmp4->next=nuovoInviato;
		    }
		}
	    }
	    call Timer0.startOneShot((((uint8_t)(call Random.rand16()))%20+10)*SEND_PERIOD);
	}
    }

    event void Timer1.fired() 
    {
	if (invioMsg==FALSE)
	{
	    nx_uint8_t oldHop;
	    Inibito* tmp=inibiti;
	    bool inibito=FALSE;
	    //Se sono giunto qui non stò gia inviando un'altro messaggio e quindi posso inoltrare il messaggio precedentemente salvato (toResend)
	    memset(&pktToSend,0,sizeof(message_t));
	    msg = (MyMsg*)(call Packet.getPayload(&pktToSend, sizeof(MyMsg)));
	    dbg("default","\n\nE' scaduta l'attesa random del numero di slot, quindi inoltro il messaggio e attendo la fine della trasmissione!\n");
	    if (msg == NULL) 
		return;
	    //Copia le informazioni riguardanti il mittente del messaggio e il sequence number mentre modifica l'informazione sull'ultimo hop che ha inoltrato il messaggio
	    memcpy(msg,daInoltrare->msg,sizeof(MyMsg));
	    //Verifico se il messaggio non è stato inibito nel frattempo (per un invio nuovo dello stesso)
	    while(tmp!=NULL)
	    {
		if (tmp->sequence_number==msg->sequence_number)
		    inibito=TRUE;
		tmp=tmp->next;
	    }
	    if (!inibito)
	    {
		oldHop=msg->hop_id;
		msg->hop_id = TOS_NODE_ID;
		if (call AMSend.send(AM_BROADCAST_ADDR, &pktToSend, sizeof(MyMsg))==SUCCESS)
		{
		    DaInoltrare* tmp2=daInoltrare;
		    SeqRicevuta* tmp3=seqRicevute;
		    Inoltrato* nuovoInoltrato=NULL;
		    Inibito* nuovoInibito=NULL;
		    //Se sono giunto qui allora il messaggio è stato preso in consegna per l'invio e quindi segnalo che c'è un messaggio in corso di invio, che la fase S2 è finita, inibisco il nodo per inviare ulteriormente il messaggio con stesso sequence number e incremento il numero di messaggi inviati
		    invioMsg=TRUE;
		    while(tmp3!=NULL)
		    {
			if (tmp3->sequence_number==msg->sequence_number)
			    tmp3->S2=FALSE;
			tmp3=tmp3->next;
		    }
		    //Inibisco il messaggio inoltrato
		    nuovoInibito=(Inibito*) malloc(sizeof(Inibito));
		    nuovoInibito->sequence_number=msg->sequence_number;
		    nuovoInibito->next=inibiti;
		    inibiti=nuovoInibito;
		    //Aggiungo il mex tra quelli inoltrati
		    nuovoInoltrato=(Inoltrato*) malloc(sizeof(Inoltrato));
		    nuovoInoltrato->msg=(MyMsg*) malloc(sizeof(MyMsg));
		    memcpy(nuovoInoltrato->msg,msg,sizeof(MyMsg));
		    nuovoInoltrato->msg->hop_id=oldHop;
		    nuovoInoltrato->next=NULL;
		    if (inoltrati==NULL)
			inoltrati=nuovoInoltrato;
		    else
		    {
			Inoltrato* tmp5=inoltrati;
			while (tmp5->next!=NULL)
			    tmp5=tmp5->next;
			tmp5->next=nuovoInoltrato;
		    }
		    //Verifico se non vi sono altri messaggi da inviare
		    if (daInoltrare->next!=NULL)
			call Timer1.startOneShot(daInoltrare->next->when-daInoltrare->when);
		    daInoltrare=daInoltrare->next;
		    free(tmp2->msg);
		    free(tmp2);
		    dbg("default","Il nodo %d è inibito nell'inviare il messaggio %d\n",TOS_NODE_ID,msg->sequence_number);
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
		//Rimuovo il mex tra quelli da inoltrare
		DaInoltrare* tmp2=daInoltrare;
		dbg("default","Il messaggio da inoltrare non deve essere più inoltrato in quanto risulta inibito\n");
		daInoltrare=daInoltrare->next;
		free(tmp2->msg);
		free(tmp2);
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
	Inviato* tmp4=inviati;
	Inoltrato* tmp5=inoltrati;
	Ricevuto* tmp6=ricevuti;
	dbg("log","Nodo %d:\n",TOS_NODE_ID);
	dbg("log","* Messaggi inviati:\n");
	if (inviati==NULL)
	    dbg("log","\tNessuno\n");
	else
	    while(tmp4!=NULL)
	    {
		dbg("log","\tSequence: %d\n",tmp4->sequence_number);
		tmp4=tmp4->next;
	    }
	dbg("log","* Messaggi inoltrati:\n");
	if (inoltrati==NULL)
	    dbg("log","\tNessuno\n");
	else
	    while(tmp5!=NULL)
	    {
		dbg("log","\tSequence: %d, da: %d, mittente: %d\n",tmp5->msg->sequence_number,tmp5->msg->hop_id,tmp5->msg->source_id);
		tmp5=tmp5->next;
	    }
	dbg("log","* Messaggi ricevuti:\n");
	if (ricevuti==NULL)
	    dbg("log","\tNessuno\n");
	else
	    while(tmp6!=NULL)
	    {
		dbg("log","\tSequence: %d, da: %d, mittente: %d\n",tmp6->msg->sequence_number,tmp6->msg->hop_id,tmp6->msg->source_id);
		tmp6=tmp6->next;
	    }
	if (TOS_NODE_ID==9)
	    dbg("log","\n\n");
    }

    event void AMControl.stopDone(error_t err) 
    {
	Inibito* tmp=inibiti;
	DaInoltrare* tmp2=daInoltrare;
	SeqRicevuta* tmp3=seqRicevute;
	Inviato* tmp4=inviati;
	Inoltrato* tmp5=inoltrati;
	Ricevuto* tmp6=ricevuti;
	while (tmp!=NULL)
	{
	    inibiti=inibiti->next;
	    free(tmp);
	    tmp=inibiti;
	}
	while (tmp2!=NULL)
	{
	    daInoltrare=daInoltrare->next;
	    free(tmp2->msg);
	    free(tmp2);
	    tmp2=daInoltrare;
	}
	while (tmp3!=NULL)
	{
	    seqRicevute=seqRicevute->next;
	    free(tmp3);
	    tmp3=seqRicevute;
	}
	while (tmp4!=NULL)
	{
	    inviati=inviati->next;
	    free(tmp4);
	    tmp4=inviati;
	}
	while (tmp5!=NULL)
	{
	    inoltrati=inoltrati->next;
	    free(tmp5->msg);
	    free(tmp5);
	    tmp5=inoltrati;
	}
	while (tmp6!=NULL)
	{
	    ricevuti=ricevuti->next;
	    free(tmp6->msg);
	    free(tmp6);
	    tmp6=ricevuti;
	}
	free(msg);
    }

    event void AMSend.sendDone(message_t* msg_snd, error_t err) 
    {
	//Viene stampato a video che è stato effettivamente inviato il messaggio/ack e si imposta la variabile di controllo in modo da permettere altri invii
	if (msg_snd==&pkt || msg_snd==&pktToSend)
	{
	    dbg("default","Nodo %d ha inviato un messaggio!\n",TOS_NODE_ID);
	    invioMsg=FALSE;
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
	    Inibito* tmp=inibiti;
	    bool inibito=FALSE;
	    SeqRicevuta* tmp3=seqRicevute;
	    bool ricevuto=FALSE;
	    bool S2=FALSE;
	    Ricevuto* nuovoRicevuto=NULL;
	    //Si preleva il messaggio ricevuto e se ne tiene una copia per l'eventuale rinvio in S2 e per l'ack del messaggio ricevuto
	    msg = (MyMsg*)payload;
	    //Aggiungo il messaggio tra quelli ricevuti
	    nuovoRicevuto=(Ricevuto*) malloc(sizeof(Ricevuto));
	    nuovoRicevuto->msg=(MyMsg*) malloc(sizeof(MyMsg));
	    memcpy(nuovoRicevuto->msg,msg,sizeof(MyMsg));
	    nuovoRicevuto->next=NULL;
	    if (ricevuti==NULL)
		ricevuti=nuovoRicevuto;
	    else
	    {
		Ricevuto* tmp6=ricevuti;
		while (tmp6->next!=NULL)
		    tmp6=tmp6->next;
		tmp6->next=nuovoRicevuto;
	    }
	    //Verifico se il nodo ha già inibito questo sequence number
	    while(tmp!=NULL)
	    {
		if (tmp->sequence_number==msg->sequence_number)
		    inibito=TRUE;
		tmp=tmp->next;
	    }
	    //Verifico se il nodo ha già ricevuto questo sequence number
	    while(tmp3!=NULL)
	    {
		if (tmp3->sequence_number==msg->sequence_number)
		{
		    ricevuto=TRUE;
		    S2=tmp3->S2;
		}
		tmp3=tmp3->next;
	    }
	    if (!ricevuto && !inibito)
	    {
		//Il messaggio ha sequence number diverso dall'ultimo ricevuto quindi sono nella fase S1
		//uint8_t n_slot=0;
		//Calcolo la distanza utilizzando la formula : rssi=-(log10(distanza)*10*n+a)
		float distanza=pow(10,(float)(-a-rssi)/(10*n));
		SeqRicevuta* nuovaSeqRicevuta=(SeqRicevuta*) malloc(sizeof(SeqRicevuta));
		//Inizializzo la variabile dmin al valore appena calcolato per la distanza (essendo la prima volta che ricevo questo messaggio) e mi salvo il suo sequence number come ultimo messaggio ricevuto
		dmin=distanza;
		tmp3=seqRicevute;
		nuovaSeqRicevuta->sequence_number=msg->sequence_number;
		nuovaSeqRicevuta->S2=FALSE;
		nuovaSeqRicevuta->next=tmp3;
		seqRicevute=nuovaSeqRicevuta;
		dbg("default","S1 -> Ho ricevuto il messaggio %d da %d e mittente %d con RSSI %d ottenendo una stima della distanza di %f\n",msg->sequence_number,msg->hop_id,msg->source_id,rssi,distanza);
		//Se la distanza minima non soddisfa le richieste passo alla fase S5 per non inoltrare il messaggio msg (non coprirei molta area in più)
		if (dmin<D)
		{
		    Inibito* nuovoInibito=(Inibito*) malloc(sizeof(Inibito));
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltro il messaggio msg (impostando la variabile inibito)
		    call Timer1.stop();
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		    nuovoInibito->sequence_number=msg->sequence_number;
		    nuovoInibito->next=inibiti;
		    inibiti=nuovoInibito;
		}
		else
		{
		    DaInoltrare* nuovoDaInoltrare=NULL;
		    bool daAggiornare=FALSE;
		    //Se sono arrivato qui allora il messaggio è arrivato per la prima volta da una sorgente che ha una distanza superiore a quella minima e quindi posso passare alla fase S2 (quindi inizializzo a TRUE la rispettiva variabile booleana) per tentare una ritrasmissione del messaggio msg
		    tmp3=seqRicevute;
		    while(tmp3!=NULL)
		    {
			if (tmp3->sequence_number==msg->sequence_number)
			    tmp3->S2=TRUE;
			tmp3=tmp3->next;
		    }
		    //Ottengo un numero random di slot (di durata RESEND_PERIOD) per l'attesa di eventuali messaggi da altri nodi con distanza minore
		    //n_slot=((uint8_t)(call Random.rand16()))%20+10;
		    //Si aggiunge il messaggio tra quelli che devono essere inoltrati
		    nuovoDaInoltrare=(DaInoltrare*) malloc(sizeof(DaInoltrare));
		    nuovoDaInoltrare->msg=(MyMsg*) malloc(sizeof(MyMsg));
		    memcpy(nuovoDaInoltrare->msg,msg,sizeof(MyMsg));
		    nuovoDaInoltrare->when=call Timer1.getNow()+n_slot*RESEND_PERIOD;
		    nuovoDaInoltrare->next=NULL;
		    if (daInoltrare==NULL)
			daInoltrare=nuovoDaInoltrare;
		    else if (daInoltrare->when>nuovoDaInoltrare->when)
		    {
			//E' in prima posizione
			nuovoDaInoltrare->next=daInoltrare;
			daInoltrare=nuovoDaInoltrare;
			daAggiornare=TRUE;
		    }
		    else
		    {
			//E' nelle successive posizioni
			DaInoltrare* tmp2=daInoltrare;
			while (tmp2->next!=NULL && tmp2->next->when<nuovoDaInoltrare->when)
			    tmp2=tmp2->next;
			if (tmp2->next==NULL)
			    //E' in ultima posizione
			    tmp2->next=nuovoDaInoltrare;
			else
			{
			    nuovoDaInoltrare->next=tmp2->next;
			    tmp2->next=nuovoDaInoltrare;
			}
		    }
		    dbg("default","Attesa di %d slot\n",n_slot);
		    if (call Timer1.isRunning()==FALSE)
			call Timer1.startOneShot(n_slot*RESEND_PERIOD);
		    else if (daAggiornare==TRUE)
		    {
			//Cambio lo scadere del timer visto che ora deve scattare prima
			call Timer1.stop();
			call Timer1.startOneShot(daInoltrare->when-call Timer1.getNow());
		    }
		}
	    }
	    else if (ricevuto && S2==TRUE && !inibito)
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
		    Inibito* nuovoInibito=(Inibito*) malloc(sizeof(Inibito));
		    dbg("default","Non rispetta il limite, passo a S5\n");
		    //Interrompo la trasmissione se sono nella fase S2 e non inoltro il messaggio msg (impostando la variabile inibito)
		    if (S2==TRUE)
			call AMSend.cancel(msg_gen);
		    nuovoInibito->sequence_number=msg->sequence_number;
		    nuovoInibito->next=inibiti;
		    inibiti=nuovoInibito;
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
		dbg("default","Ho ricevuto il messaggio %d da %d e mittente %d ma non era conforme per l'invio\n",msg->sequence_number,msg->hop_id,msg->source_id);
	    }
	}
	return msg_gen;
    }
}
