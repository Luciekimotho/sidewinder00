import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class EventChannelImpl extends EventChannelPOA
{
    ORB orb=null;
    org.omg.CORBA.Object refSupplierAdmin=null;
    org.omg.CORBA.Object refConsumerAdmin=null;

    public EventChannelImpl(ORB orb)
    {
	this.orb=orb;
    }

    public ConsumerAdmin for_consumers()
    {
	if (refConsumerAdmin==null)
	{
	    System.out.println("EventChannelImpl:\tRichiamato metodo for_consumers");
	    //Creo il servant
	    ConsumerAdminImpl consumerAdmin = new ConsumerAdminImpl(orb);
	    try
	    {
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		refConsumerAdmin = rootPOA.servant_to_reference(consumerAdmin);
		//Ottengo il root naming context
		org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		//Faccio il binding del riferimento al consumer admin nel naming
		NameComponent path[] = ncRef.to_name("ConsumerAdmin");
		ncRef.bind(path, refConsumerAdmin);
	    }
	    catch(Exception e)
	    {
		System.out.println("EventChannelImpl:\tATTENZIONE: Impossibile gestire la creazione di ConsumerAdmin!\nMotivo: "+e.getCause());
		consumerAdmin=null;
	    }
	}
	return ConsumerAdminHelper.narrow(refConsumerAdmin);
    };

    public SupplierAdmin for_suppliers()
    {
	if (refSupplierAdmin==null)
	{
	    System.out.println("EventChannelImpl:\tRichiamato metodo for_suppliers");
	    //Creo il servant
	    SupplierAdminImpl supplierAdmin = new SupplierAdminImpl(orb);
	    try
	    {
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		refSupplierAdmin = rootPOA.servant_to_reference(supplierAdmin);
		//Ottengo il root naming context
		org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		//Faccio il binding del riferimento al supplier admin nel naming
		NameComponent path[] = ncRef.to_name("SupplierAdmin");
		ncRef.bind(path, refSupplierAdmin);
	    } 
	    catch(Exception e) 
	    { 
		System.out.println("EventChannelImpl:\tATTENZIONE: Impossibile gestire la creazione di SupplierAdmin!\nMotivo: "+e.getCause());
		supplierAdmin=null;
	    }
	}
	return SupplierAdminHelper.narrow(refSupplierAdmin);
    };

    public void destroy()
    {
	System.out.println("EventChannelImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio l'unbinding del riferimento dell'event channel dal naming
	    NameComponent path[] = ncRef.to_name("EventChannel");
	    ncRef.unbind(path);
	    //Ottengo l'id del servante e lo disattivo nel rootPOA
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	    if (refConsumerAdmin!=null)
	    {
		//Faccio l'unbinding del riferimento del consumer admin dal naming
		path = ncRef.to_name("ConsumerAdmin");
		ncRef.unbind(path);
		//Ottengo il servante consumer admin dal suo riferimento, chiamo il metodo destroy su di esso e lo disattivo nel rootPOA
		ConsumerAdminImpl consumerAdmin=(ConsumerAdminImpl)rootPOA.reference_to_servant(refConsumerAdmin);
		consumerAdmin.destroy();
		id=rootPOA.servant_to_id(consumerAdmin);
		rootPOA.deactivate_object(id);
		refConsumerAdmin=null;
	    }
	    if (refSupplierAdmin!=null)
	    {
		//Faccio l'unbinding del riferimento del supplier admin dal naming
		path = ncRef.to_name("SupplierAdmin");
		ncRef.unbind(path);
		//Ottengo il servante supplier admin dal suo riferimento, chiamo il metodo destroy su di esso e lo disattivo nel rootPOA
		SupplierAdminImpl supplierAdmin=(SupplierAdminImpl)rootPOA.reference_to_servant(refSupplierAdmin);
		supplierAdmin.destroy();
		id=rootPOA.servant_to_id(supplierAdmin);
		rootPOA.deactivate_object(id);
		refSupplierAdmin=null;
	    }
	} 
	catch(Exception e) 
	{ 
	    System.out.println("EventChannelImpl:\tATTENZIONE: Impossibile gestire la distruzione dell'event channel e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    };
}