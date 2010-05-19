import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class TypedEventChannelImpl extends TypedEventChannelPOA
{
    ORB orb=null;
    org.omg.CORBA.Object refTypedSupplierAdmin=null;
    org.omg.CORBA.Object refTypedConsumerAdmin=null;

    public TypedEventChannelImpl(ORB orb)
    {
	this.orb=orb;
    }

    public TypedConsumerAdmin for_consumers()
    {
	if (refTypedConsumerAdmin==null)
	{
	    System.out.println("TypedEventChannelImpl:\tRichiamato metodo for_consumers");
	    //Creo il servant
	    TypedConsumerAdminImpl typedConsumerAdmin = new TypedConsumerAdminImpl(orb);
	    try
	    {
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		refTypedConsumerAdmin = rootPOA.servant_to_reference(typedConsumerAdmin);
		//Ottengo il root naming context
		org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		//Faccio il binding del riferimento al supplier nel naming
		NameComponent path[] = ncRef.to_name("TypedConsumerAdmin");
		ncRef.bind(path, refTypedConsumerAdmin);
	    }
	    catch(Exception e)
	    {
		System.out.println("TypedEventChannelImpl:\tATTENZIONE: Impossibile gestire la creazione di TypedConsumerAdmin!\nMotivo: "+e.getCause());
		typedConsumerAdmin=null;
	    }
	}
	return TypedConsumerAdminHelper.narrow(refTypedConsumerAdmin);
    };

    public TypedSupplierAdmin for_suppliers()
    {
	if (refTypedSupplierAdmin==null)
	{
	    System.out.println("TypedEventChannelImpl:\tRichiamato metodo for_suppliers");
	    //Creo il servant
	    TypedSupplierAdminImpl typedSupplierAdmin = new TypedSupplierAdminImpl(orb);
	    try
	    {
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		refTypedSupplierAdmin = rootPOA.servant_to_reference(typedSupplierAdmin);
		//Ottengo il root naming context
		org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		//Faccio il binding del riferimento al supplier nel naming
		NameComponent path[] = ncRef.to_name("TypedSupplierAdmin");
		ncRef.bind(path, refTypedSupplierAdmin);
	    } 
	    catch(Exception e) 
	    { 
		System.out.println("TypedEventChannelImpl:\tATTENZIONE: Impossibile gestire la creazione di TypedSupplierAdmin!\nMotivo: "+e.getCause());
		typedSupplierAdmin=null;
	    }
	}
	return TypedSupplierAdminHelper.narrow(refTypedSupplierAdmin);
    };

    public void destroy()
    {
	System.out.println("TypedEventChannelImpl:\tRichiamato metodo destroy");
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio il binding del riferimento al supplier nel naming
	    NameComponent path[] = ncRef.to_name("TypedEventChannel");
	    ncRef.unbind(path);
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	    if (refTypedConsumerAdmin!=null)
	    {
		//Faccio il binding del riferimento al supplier nel naming
		path = ncRef.to_name("TypedConsumerAdmin");
		ncRef.unbind(path);
		TypedConsumerAdminImpl typedConsumerAdmin=(TypedConsumerAdminImpl)rootPOA.reference_to_servant(refTypedConsumerAdmin);
		typedConsumerAdmin.destroy();
		id=rootPOA.servant_to_id(typedConsumerAdmin);
		rootPOA.deactivate_object(id);
		refTypedConsumerAdmin=null;
	    }
	    if (refTypedSupplierAdmin!=null)
	    {
		//Faccio il binding del riferimento al supplier nel naming
		path = ncRef.to_name("TypedSupplierAdmin");
		ncRef.unbind(path);
		TypedSupplierAdminImpl typedSupplierAdmin=(TypedSupplierAdminImpl)rootPOA.reference_to_servant(refTypedSupplierAdmin);
		typedSupplierAdmin.destroy();
		id=rootPOA.servant_to_id(typedSupplierAdmin);
		rootPOA.deactivate_object(id);
		refTypedSupplierAdmin=null;
	    }
	} 
	catch(Exception e) 
	{ 
	    System.out.println("TypedEventChannelImpl:\tATTENZIONE: Impossibile gestire la distruzione del typed event channel e di tutto ciò che è a lui collegato!\nMotivo: "+e.getCause());
	}
    };
}