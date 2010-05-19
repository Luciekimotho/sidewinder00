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

    public void push (int data) throws Disconnected
    {
	System.out.println("ProxyPushConsumerImpl:\tRichiamato metodo push con data: "+data);
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il riferimento all'oggetto dal naming
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedConsumerAdmin");
	    if (objRef!=null)
	    {
		TypedConsumerAdminImpl typedConsumerAdmin=(TypedConsumerAdminImpl)rootPOA.reference_to_servant(objRef);
		List<org.omg.CORBA.Object> lista=typedConsumerAdmin.list_proxy_push_supplier();
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
	    }
	} 
	catch(Exception e) 
	{ 
	    System.out.println("ProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	}
    };

    public void disconnect_push_consumer()
    {
	System.out.println("ProxyPushConsumerImpl:\tDisconnesso ProxyPushConsumer");
	for(PushSupplier pushSupplier:listPushSupplier)
	    pushSupplier.disconnect_push_supplier();
	listPushSupplier.clear();
	//TODO Disconnettilo dal SupplierAdmin??
    };
}