import CosEventComm.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

public class PushSupplierImpl extends PushSupplierPOA
{
    ORB orb=null;
    ProxyPushConsumer consumer=null;
    ThreadPush t=null;

    public PushSupplierImpl(ORB orb,ProxyPushConsumer consumer,Any data)
    {
	this.orb=orb;
	this.consumer=consumer;
	t=new ThreadPush(consumer,data);
    }

    public void disconnect_push_supplier()
    {
	System.out.println("Disconnesso supplier");
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
	    System.out.println("ATTENZIONE: Impossibile gestire la distruzione del PushSupplier!\nMotivo: "+e.getCause());
	}
    };

    //Aggiunto io per simulare delle push
    public void start_push()
    {
	System.out.println("Richiamato start_push sul supplier "+t);
	t.start();
    };
}