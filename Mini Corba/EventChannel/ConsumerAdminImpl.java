import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class ConsumerAdminImpl extends ConsumerAdminPOA
{
    ORB orb=null;
    List<org.omg.CORBA.Object> listProxyPushSupplier=null;

    public ConsumerAdminImpl(ORB orb)
    {
	this.orb=orb;
	listProxyPushSupplier=new ArrayList<org.omg.CORBA.Object>();
    }

    public ProxyPushSupplier obtain_push_supplier()
    {
	System.out.println("ConsumerAdminImpl:\tRichiamato metodo obtain_push_supplier");
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
	    //Aggiungo il riferimento al nuovo ProxyPushConsumer alla lista
	    listProxyPushSupplier.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("ConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di ProxyPushSupplier!\nMotivo: "+e.getCause());
	    proxyPushSupplier=null;
	}
	return ProxyPushSupplierHelper.narrow(ref);
    };

    //NOTE Aggiunto per distruggere tutti i proxy quando chiamo destroy sull'event channel
    public void destroy()
    {
	System.out.println("ConsumerAdminImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Per ogni ProxyPushSupplier creato dal ConsumerAdmin (e quindi presente nella lista) chiamo il metodo disconnect_push_supplier per disconnetterlo e successivamente lo disattivo nel rootPOA
	    for (org.omg.CORBA.Object ref:listProxyPushSupplier)
	    {
		(ProxyPushSupplierHelper.narrow(ref)).disconnect_push_supplier();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    //Rimuovo tutti gli elementi dalla lista
	    listProxyPushSupplier.clear();
	}
	catch(Exception e)
	{
	    System.out.println("ConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la distruzione del consumer admin e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    }
    
    //NOTE Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public List<org.omg.CORBA.Object> list_proxy_push_supplier()
    {
	System.out.println("ConsumerAdminImpl:\tRichiamato metodo list_proxy_push_supplier");
	return new ArrayList(listProxyPushSupplier);
    }	
}