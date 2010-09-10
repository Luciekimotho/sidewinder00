import CosEventChannelAdmin.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class ProxyPushConsumerImpl extends ProxyPushConsumerPOA
{
    ORB orb=null;
    List<PushSupplier> listPushSupplier=null;

    public ProxyPushConsumerImpl(ORB orb)
    {
	this.orb=orb;
	listPushSupplier=new ArrayList<PushSupplier>();
    }

    public void connect_push_supplier(PushSupplier push_supplier) throws AlreadyConnected
    {
	System.out.println("ProxyPushConsumerImpl:\tRichiamato metodo connect_push_supplier con push_supplier: "+push_supplier);
	if (listPushSupplier.contains(push_supplier))
	    throw new AlreadyConnected();
	else if (push_supplier!=null)
	    listPushSupplier.add(push_supplier);
    };

    public void push(Any data) throws Disconnected
    {
	System.out.println("ProxyPushConsumerImpl:\tRichiamato metodo push con data: "+data);
	POA rootPOA = null;
	org.omg.CORBA.Object objRef = null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il riferimento del ConsumerAdmin dal naming
	    objRef = orb.string_to_object("corbaname::localhost:1050#ConsumerAdmin");
	}
	catch(Exception e) 
	{
	    System.out.println("ProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    objRef=null;
	}
	if (objRef!=null)
	{
	    List<org.omg.CORBA.Object> lista=null;
	    try
	    {
		//Ottengo il servante dal riferimento che è stato ottenuto dal naming
		ConsumerAdminImpl consumerAdmin=(ConsumerAdminImpl)rootPOA.reference_to_servant(objRef);
		//Prelevo la lista di tutti i ProxyPushSupplier connessi al ConsumerAdmin
		lista=consumerAdmin.list_proxy_push_supplier();
	    }
	    catch(Exception e) 
	    {
		System.out.println("ProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    }
	    //Per ogni ProxyPushSupplier presente nella lista richiamo su di esso il metodo send_data per permettere di inoltrare i dati creati dal supplier a tutti i consumer connessi a quel ProxyPushSupplier
	    for (org.omg.CORBA.Object ref:lista)
		try
		{
		    ((ProxyPushSupplierImpl)rootPOA.reference_to_servant(ref)).send_data(data);
		} 
		catch(Disconnected d) 
		{ 
		    System.out.println("ProxyPushConsumerImpl:\tATTENZIONE: Qualche consumer non ha ricevuto l'informazione in quanto disconnesso!");
		    //Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		    throw d;
		}
		catch(Exception e) 
		{
		    System.out.println("ProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
		    objRef=null;
		}
	}
    };

    public void disconnect_push_consumer()
    {
	System.out.println("ProxyPushConsumerImpl:\tDisconnesso ProxyPushConsumer");
	//Per ogni PushSupplier collegato al ProxyPushConsumer (e quindi presente nella lista) chiamo il metodo disconnect_push_supplier per disconnetterlo
	for(PushSupplier pushSupplier:listPushSupplier)
	    pushSupplier.disconnect_push_supplier();
	//Rimuovo tutti gli elementi dalla lista
	listPushSupplier.clear();
    };
}