import CosTypedEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class TypedPushConsumerServer 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo ORB
	    ORB orb = ORB.init(args, null);
	    //Creo il servant
	    TypedPushConsumerImpl consumer = new TypedPushConsumerImpl(orb);
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    org.omg.CORBA.Object ref = rootPOA.servant_to_reference(consumer);
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio il binding del riferimento al supplier nel naming
	    //TODO Mettere nomi univoci
	    NameComponent path[] = ncRef.to_name("TypedConsumer");
	    ncRef.rebind(path, ref);
	    //Attendo le invocazioni da parte dei consumer (SOLO DISCONNESSIONI)
	    System.out.println("Typed Consumer server pronto e in attesa di chiamate dai supplier client...");
	    orb.run();
	} 
	catch(Exception e) 
	{ 
	    e.printStackTrace(); 
	}
    }
}
