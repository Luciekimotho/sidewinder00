import Type.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class IProxyImpl extends IPOA
{
    ORB orb=null;

    public IProxyImpl(ORB orb)
    {
	this.orb=orb;
    }

    public void pushString (String data) throws Disconnected
    {
	System.out.println("IProxyImpl:\tRichiamato metodo pushString con data: "+data);
	//Incapsulo la stringa nell'any e la mando al typed proxy che la smisterà poi nuovamente come stringa ai consumer tipizzati
	Any any=orb.create_any();
	any.insert_string(data);
	POA rootPOA = null;
	org.omg.CORBA.Object objRef = null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il riferimento del TypedConsumerAdmin dal naming
	    objRef = orb.string_to_object("corbaname::localhost:1050#TypedConsumerAdmin");
	}
	catch(Exception e) 
	{
	    System.out.println("IProxyImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    objRef=null;
	}
	if (objRef!=null)
	{
	    List<org.omg.CORBA.Object> lista=null;
	    try
	    {
		//Ottengo il servante dal riferimento che è stato ottenuto dal naming
		TypedConsumerAdminImpl typedConsumerAdmin=(TypedConsumerAdminImpl)rootPOA.reference_to_servant(objRef);
		//Prelevo la lista di tutti i TypedProxyPushSupplier e quindi che gestisce i consumer tipizzati che sono connessi al TypedConsumerAdmin
		lista=typedConsumerAdmin.list_typed_proxy_push_supplier();
	    }
	    catch(Exception e) 
	    {
		System.out.println("IProxyImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    }
	    //Per ogni TypedProxyPushSupplier presente nella lista richiamo su di esso il metodo send_data per permettere di inoltrare i dati creati dal supplier a tutti i consumer connessi a quel TypedProxyPushSupplier
	    for (org.omg.CORBA.Object ref:lista)
		try
		{
		    ((TypedProxyPushSupplierImpl)rootPOA.reference_to_servant(ref)).send_data(any);
		} 
		catch(Disconnected d) 
		{ 
		    System.out.println("IProxyImpl:\tATTENZIONE: Qualche consumer non ha ricevuto l'informazione in quanto disconnesso!");
		    //Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		    throw d;
		}
		catch(Exception e) 
		{
		    System.out.println("IProxyImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
		    objRef=null;
		}
	}
    };

    public void disconnect_push_consumer()
    {
	System.out.println("IProxyImpl:\tDisconnesso IProxyImpl");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Disattivo nel rootPOA l'implementazione proxy dell'interfaccia
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	}
	catch(Exception e)
	{
	    System.out.println("IProxyImpl:\tATTENZIONE: Impossibile gestire la distruzione di IProxyImpl!\nMotivo: "+e.getCause());
	}
    };
}