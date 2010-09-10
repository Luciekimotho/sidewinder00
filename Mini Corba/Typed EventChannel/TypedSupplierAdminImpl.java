import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import java.util.*;

public class TypedSupplierAdminImpl extends TypedSupplierAdminPOA
{
    ORB orb=null;
    List<org.omg.CORBA.Object> listAllProxyPushConsumer=null;

    public TypedSupplierAdminImpl(ORB orb)
    {
	this.orb=orb;
	listAllProxyPushConsumer=new ArrayList<org.omg.CORBA.Object>();
    }

    public TypedProxyPushConsumer obtain_typed_push_consumer(String supported_interface) throws InterfaceNotSupported
    {
	System.out.println("TypedSupplierAdminImpl:\tRichiamato metodo obtain_typed_push_consumer");
	//Verifico se l'interfaccia è supportata, se non lo è lancio l'eccezzione
	if (!supported_interface.equals("IString"))
	    throw new InterfaceNotSupported();
	//Creo il servant
	TypedProxyPushConsumerImpl typedProxyPushConsumer = new TypedProxyPushConsumerImpl(orb);
	org.omg.CORBA.Object ref=null;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    ref = rootPOA.servant_to_reference(typedProxyPushConsumer);
	    //Aggiungo il riferimento al nuovo TypedProxyPushConsumer alla lista
	    listAllProxyPushConsumer.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedSupplierAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di TypedProxyPushConsumer!\nMotivo: "+e.getCause());
	    typedProxyPushConsumer=null;
	}
	return TypedProxyPushConsumerHelper.narrow(ref);
    }

    public ProxyPushConsumer obtain_push_consumer()
    {
	System.out.println("TypedSupplierAdminImpl:\tRichiamato metodo obtain_push_consumer");
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
	    //Aggiungo il riferimento al nuovo ProxyPushConsumer alla lista
	    listAllProxyPushConsumer.add(ref);
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedSupplierAdminImpl:\tATTENZIONE: Impossibile gestire la creazione di ProxyPushConsumer!\nMotivo: "+e.getCause());
	    proxyPushConsumer=null;
	}
	return ProxyPushConsumerHelper.narrow(ref);
    };
    
    //NOTE Aggiunto per distruggere tutti i proxy quando chiamo destroy sull'event channel
    public void destroy()
    {
	System.out.println("TypedSupplierAdminImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Per ogni ProxyPushConsumer e TypedProxyPushConsumer creato dal TypedSupplierAdmin (e quindi presente nella lista) chiamo il metodo disconnect_push_consumer per disconnetterlo e successivamente lo disattivo nel rootPOA 
	    for (org.omg.CORBA.Object ref:listAllProxyPushConsumer)
	    {
		//Casto anche i typed alla forma generica visto che tanto è utilizzato solo il metodo disconnect che è ereditato da quest'ultima
		(ProxyPushConsumerHelper.narrow(ref)).disconnect_push_consumer();
		byte[] id=rootPOA.reference_to_id(ref);
		rootPOA.deactivate_object(id);
	    }
	    //Rimuovo tutti gli elementi dalla lista
	    listAllProxyPushConsumer.clear();
	}
	catch(Exception e)
	{
	    System.out.println("TypedConsumerAdminImpl:\tATTENZIONE: Impossibile gestire la distruzione del typed supplier admin e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    }
}