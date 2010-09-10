import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import CosEventComm.*;
import Type.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class TypedProxyPushConsumerImpl extends TypedProxyPushConsumerPOA
{
    ORB orb=null;
    List<PushSupplier> listPushSupplier=null;
    org.omg.CORBA.Object iRef=null;

    public TypedProxyPushConsumerImpl(ORB orb)
    {
	this.orb=orb;
	listPushSupplier=new ArrayList<PushSupplier>();
    }

    public org.omg.CORBA.Object get_typed_consumer()
    {
	if (iRef==null)
	{
	    try
	    {
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Istanzio l'implementazione dell'interfaccia I che verrà utilizzata come proxy dal push supplier per smistare le push tipizzate ai push consumer in grando di leggerle
		IProxyImpl i=new IProxyImpl(orb);
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		iRef = rootPOA.servant_to_reference(i);
	    }
	    catch(Exception e)
	    {
		System.out.println("TypedProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire la creazione dell'interfaccia tipata!\nMotivo: "+e.getCause());
	    }
	}
	return iRef;
    }

    public void connect_push_supplier(PushSupplier push_supplier) throws AlreadyConnected
    {
	System.out.println("TypedProxyPushConsumerImpl:\tRichiamato metodo connect_push_supplier con push_supplier: "+push_supplier);
	//Verifico se il push supplier non è gia presente nella lista, se lo è rilancio l'eccezzione
	if (listPushSupplier.contains(push_supplier))
	    throw new AlreadyConnected();
	//Se il push supplier non è null viene inserito nella lista in modo da essere contattato per la disconnessione
	else if (push_supplier!=null)
	    listPushSupplier.add(push_supplier);
    };
	
    public void push(org.omg.CORBA.Any data) throws Disconnected
    {
	//Metodo offerto per le chiamate generiche, avrei potuto anche non implementarla se voglio usare solo typed, in tal caso avrei lanciato l'eccezzione NO_IMPLEMENT
	System.out.println("TypedProxyPushConsumerImpl:\tRichiamato metodo push con data: "+data.extract_long());
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
	    System.out.println("TypedProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    objRef=null;
	}
	if (objRef!=null)
	{
	    List<org.omg.CORBA.Object> lista=null;
	    try
	    {
		//Ottengo il servante dal riferimento che è stato ottenuto dal naming
		TypedConsumerAdminImpl typedConsumerAdmin=(TypedConsumerAdminImpl)rootPOA.reference_to_servant(objRef);
		//Prelevo la lista di tutti i ProxyPushSupplier, e quindi generici, connessi al TypedConsumerAdmin
		lista=typedConsumerAdmin.list_proxy_push_supplier();
	    }
	    catch(Exception e) 
	    {
		System.out.println("TypedProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
	    }
	    //Per ogni ProxyPushSupplier presente nella lista richiamo su di esso il metodo send_data per permettere di inoltrare i dati creati dal supplier a tutti i consumer connessi a quel ProxyPushSupplier
	    for (org.omg.CORBA.Object ref:lista)
		try
		{
		    ((ProxyPushSupplierImpl)rootPOA.reference_to_servant(ref)).send_data(data);
		} 
		catch(Disconnected d) 
		{ 
		    System.out.println("TypedProxyPushConsumerImpl:\tATTENZIONE: Qualche consumer non ha ricevuto l'informazione in quanto disconnesso!");
		    //Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		    throw d;
		}
		catch(Exception e) 
		{
		    System.out.println("TypedProxyPushConsumerImpl:\tATTENZIONE: Impossibile gestire l'inoltro delle informazioni ai consumer!\nMotivo: "+e.getCause());
		    objRef=null;
		}
	}
    };

    public void disconnect_push_consumer()
    {
	System.out.println("TypedProxyPushConsumerImpl:\tDisconnesso TypedProxyPushConsumer");
	//Disconnetto tutti i PushSupplier presenti in lista in modo da informarli della disconnessione
	for(PushSupplier pushSupplier:listPushSupplier)
	    pushSupplier.disconnect_push_supplier();
	listPushSupplier.clear();
	if (iRef!=null)
	    //Richiamo la disconnessione anche sull'implementazione proxy dell'interfaccia creata
	    (IHelper.narrow(iRef)).disconnect_push_consumer();;
    };
}