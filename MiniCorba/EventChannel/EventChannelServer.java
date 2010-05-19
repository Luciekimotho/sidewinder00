import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class EventChannelServer 
{
    public static void main(String args[]) 
    {
	//Creo e inizializzo ORB
	ORB orb = ORB.init(args, null);
	//Creo il servant
	EventChannelImpl eventChannel = new EventChannelImpl(orb);
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    org.omg.CORBA.Object ref = rootPOA.servant_to_reference(eventChannel);
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio il binding del riferimento al supplier nel naming
	    NameComponent path[] = ncRef.to_name("EventChannel");
	    ncRef.bind(path, ref);
	    //Attendo le invocazioni
	    System.out.println("Event channel server pronto e in attesa di chiamate...");
	    orb.run();  
	}
	catch(Exception e)
	{ 
	    System.out.println("EventChannelServer:\tATTENZIONE: Impossibile avviare il server e registrarlo!\nMotivo: "+e.getCause());
	    eventChannel=null;
	    orb=null;
	    System.exit(-1);
	}
    }
}
