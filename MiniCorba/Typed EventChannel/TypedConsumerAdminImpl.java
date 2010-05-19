import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class TypedConsumerAdminImpl extends TypedConsumerAdminPOA
{
    ORB orb=null;
    List<org.omg.CORBA.Object> listProxyPushSupplier=null;

    public TypedConsumerAdminImpl(ORB orb)
    {
	this.orb=orb;
	listProxyPushSupplier=new ArrayList<org.omg.CORBA.Object>();
    }

    public ProxyPushSupplier obtain_typed_push_supplier(String uses_interface) throws NoSuchImplementation
    {
	return null;
    }

    public ProxyPushSupplier obtain_push_supplier()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo obtain_push_supplier");
	//Creo il servant
	ProxyPushSupplierImpl proxyPushSupplier = new ProxyPushSupplierImpl(orb);
	org.omg.CORBA.Object ref=null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    ref = rootPOA.servant_to_reference(proxyPushSupplier);
	    listProxyPushSupplier.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di ProxyPushSupplier!\nMotivo: "+e.getCause());
	    proxyPushSupplier=null;
	}
	return ProxyPushSupplierHelper.narrow(ref);
    };

    //TODO Aggiunto per distruggere tutti i proxy quando chiamo destroy sul event channel
    public void destroy()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    for (org.omg.CORBA.Object ref:listProxyPushSupplier)
	    {
		(ProxyPushSupplierHelper.narrow(ref)).disconnect_push_supplier();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    listProxyPushSupplier.clear();
	}
	catch(Exception e)
	{
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la distruzione del consumer admin e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    }
    
    //TODO Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public List<org.omg.CORBA.Object> list_proxy_push_supplier()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo list_proxy_push_supplier");
	return listProxyPushSupplier;
    }
}