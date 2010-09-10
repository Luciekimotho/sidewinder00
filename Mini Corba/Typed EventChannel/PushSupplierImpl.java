import CosEventComm.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

public class PushSupplierImpl extends PushSupplierPOA
{
    ORB orb=null;
    ThreadPush t=null;

    public PushSupplierImpl(ORB orb,ProxyPushConsumer consumer)
    {
	this.orb=orb;
	//Istanzio il thread di generazione delle push generiche che verrà poi attivato per generarle in modo random
	t=new ThreadPush(consumer,orb.create_any());
    }

    public void disconnect_push_supplier()
    {
	System.out.println("PushSupplierImpl:\tDisconnesso supplier");
	//Imposto la variabile a false per far terminare il ciclo del thread di generazione
	t.attivo=false;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Disattivo nel rootPOA il PushSupplier
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	}
	catch(Exception e)
	{
	    System.out.println("PushSupplierImpl:\tATTENZIONE: Impossibile gestire la distruzione del PushSupplier!\nMotivo: "+e.getCause());
	}
    };

    //NOTE Aggiunto io per simulare delle push avviando il thread
    public void start_push()
    {
	System.out.println("PushSupplierImpl:\tRichiamato start_push sul supplier");
	t.start();
    };
}