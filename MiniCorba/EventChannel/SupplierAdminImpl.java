import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class SupplierAdminImpl extends SupplierAdminPOA
{
    ORB orb=null;
    List<org.omg.CORBA.Object> listProxyPushConsumer=null;

    public SupplierAdminImpl(ORB orb)
    {
	this.orb=orb;
	listProxyPushConsumer=new ArrayList<org.omg.CORBA.Object>();
    }

    public ProxyPushConsumer obtain_push_consumer()
    {
	System.out.println("SupplierAdminImpl:\tRichiamato metodo obtain_push_consumer");
	//Creo il servant
	ProxyPushConsumerImpl proxyPushConsumer = new ProxyPushConsumerImpl(orb);
	org.omg.CORBA.Object ref=null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    ref = rootPOA.servant_to_reference(proxyPushConsumer);
	    listProxyPushConsumer.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("SupplierAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di ProxyPushConsumer!\nMotivo: "+e.getCause());
	    proxyPushConsumer=null;
	}
	return ProxyPushConsumerHelper.narrow(ref);
    };
    
    //TODO Aggiunto per distruggere tutti i proxy quando chiamo destroy sul event channel
    public void destroy()
    {
	System.out.println("SupplierAdminImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();    
	    for (org.omg.CORBA.Object ref:listProxyPushConsumer)
	    {
		(ProxyPushConsumerHelper.narrow(ref)).disconnect_push_consumer();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    listProxyPushConsumer.clear();
	}
	catch(Exception e)
	{
	    System.out.println("ConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la distruzione del consumer admin e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    }
}