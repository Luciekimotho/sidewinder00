import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class TypedConsumerAdminImpl extends TypedConsumerAdminPOA
{
    ORB orb=null;
    //Lista contenente i proxy supplier per i push consumer generici
    List<org.omg.CORBA.Object> listProxyPushSupplier=null;
    //Lista contenente i proxy supplier per i push consumer tipizzati
    List<org.omg.CORBA.Object> listTypedProxyPushSupplier=null;

    public TypedConsumerAdminImpl(ORB orb)
    {
	this.orb=orb;
	listProxyPushSupplier=new ArrayList<org.omg.CORBA.Object>();
	listTypedProxyPushSupplier=new ArrayList<org.omg.CORBA.Object>();
    }

    public ProxyPushSupplier obtain_typed_push_supplier(String uses_interface) throws NoSuchImplementation
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo obtain_typed_push_supplier");
	//Verifico se l'interfaccia che si desidera usare è tra quelle supportate, in caso contrario lancio l'eccezzione
	if (!uses_interface.equals("IString"))
	    throw new NoSuchImplementation();
	//Creo il servant
	TypedProxyPushSupplierImpl typedProxyPushSupplier = new TypedProxyPushSupplierImpl(orb);
	org.omg.CORBA.Object ref=null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    ref = rootPOA.servant_to_reference(typedProxyPushSupplier);
	    //Aggiungo il riferimento al nuovo TypedProxyPushSupplier alla lista
	    listTypedProxyPushSupplier.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di TypedProxyPushSupplier!\nMotivo: "+e.getCause());
	    typedProxyPushSupplier=null;
	}
	return ProxyPushSupplierHelper.narrow(ref);
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
	    //Aggiungo il riferimento al nuovo ProxyPushSupplier alla lista
	    listProxyPushSupplier.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di ProxyPushSupplier!\nMotivo: "+e.getCause());
	    proxyPushSupplier=null;
	}
	return ProxyPushSupplierHelper.narrow(ref);
    };

    //NOTE Aggiunto per distruggere tutti i proxy quando chiamo destroy sull'event channel
    public void destroy()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Per ogni ProxyPushSupplier creato dal TypedConsumerAdmin (e quindi presente nella lista) chiamo il metodo disconnect_push_supplier per disconnetterlo e successivamente lo disattivo nel rootPOA
	    for (org.omg.CORBA.Object ref:listProxyPushSupplier)
	    {
		(ProxyPushSupplierHelper.narrow(ref)).disconnect_push_supplier();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    //Rimuovo tutti gli elementi dalla lista
	    listProxyPushSupplier.clear();
	    //Per ogni TypedProxyPushSupplier creato dal TypedConsumerAdmin (e quindi presente nella lista) chiamo il metodo disconnect_push_supplier per disconnetterlo e successivamente lo disattivo nel rootPOA
	    for (org.omg.CORBA.Object ref:listTypedProxyPushSupplier)
	    {
		(ProxyPushSupplierHelper.narrow(ref)).disconnect_push_supplier();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    //Rimuovo tutti gli elementi dalla lista
	    listTypedProxyPushSupplier.clear();
	}
	catch(Exception e)
	{
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la distruzione del typed consumer admin e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    }
    
    //NOTE Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public List<org.omg.CORBA.Object> list_proxy_push_supplier()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo list_proxy_push_supplier");
	//Manda una copia della lista interna al richiedente, utilizzata poi per inoltrare le push dai supplier ai consumer generici
	return new ArrayList(listProxyPushSupplier);
    }	

    //NOTE Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public List<org.omg.CORBA.Object> list_typed_proxy_push_supplier()
    {
	System.out.println("TypedConsumerAdminImpl:\tRichiamato metodo list_typed_proxy_push_supplier");
	//Manda una copia della lista interna al richiedente, utilizzata poi per inoltrare le push dai supplier ai consumer tipizzati
	return new ArrayList(listTypedProxyPushSupplier);
    }	
}