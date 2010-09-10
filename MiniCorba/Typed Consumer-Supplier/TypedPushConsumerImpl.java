import CosTypedEventComm.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class TypedPushConsumerImpl extends TypedPushConsumerPOA
{
    int num=0;
    int somma=0;
    ORB orb=null;
    org.omg.CORBA.Object ref=null;
    IImpl i=null;

    public TypedPushConsumerImpl(ORB orb)
    {
	this.orb=orb;
    }
    
    public org.omg.CORBA.Object get_typed_consumer()
    {
	if (ref==null)
	{
	    try
	    {
		//throw new org.omg.CORBA.BAD_INV_ORDER();
		//Istanzio l'interfaccia gestita dal consumer e che accetterà le richieste del supplier
		i=new IImpl();
		//Ottengo il riferimento al rootPOA e attivo il POAManager
		POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		rootPOA.the_POAManager().activate();
		//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
		ref = rootPOA.servant_to_reference(i);
	    }
	    catch(Exception e)
	    {
		System.out.println("ATTENZIONE: Impossibile gestire la creazione dell'interfaccia tipata!\nMotivo: "+e.getCause());
	    }
	}
	return ref;
    };

    public void push(org.omg.CORBA.Any data) throws Disconnected
    {
	//Potrei anche non implementarla se voglio usare solo typed, in tal caso lancia l'eccezzione NO_IMPLEMENT
	System.out.println("Richiamato metodo push con data: "+data.extract_long()+" sul consumer");
	somma=somma+data.extract_long();
	num++;
    };

    public void disconnect_push_consumer()
    {
	System.out.println("Disconnesso consumer");
	if (num!=0)
	    System.out.println("Media: "+(double)somma/num);
	num=0;
	somma=0;
	try
	{
	    if (i!=null)
		i.change_state();
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio l'unbinding del riferimento al consumer nel naming
	    NameComponent path[] = ncRef.to_name("TypedPushConsumer");
	    ncRef.unbind(path);
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	}
	catch(Exception e)
	{
	    System.out.println("ATTENZIONE: Impossibile gestire la disconnessione del TypedPushConsumer!\nMotivo: "+e.getCause());
	}
    };
}