import CosEventComm.*;
import org.omg.CORBA.*;

public class PushSupplierClient 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo l'ORB
	    ORB orb = ORB.init(args, null);
	    //Ottengo il riferimento all'oggetto dal naming
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#Consumer");
	    if (objRef==null)
	    {
		System.out.println("ATTENZIONE: Nessun Consumer individuato!");
		System.exit(-1);
	    }
	    PushConsumer consumer = PushConsumerHelper.narrow(objRef);
	    //Creo il servant
	    Any data=orb.create_any();
	    PushSupplierImpl supplier=new PushSupplierImpl(consumer,data);
	    System.out.println("Premi invio per avviare il generatore di push e ripremilo per disconnettere il supplier");
	    System.console().readLine();
	    supplier.start_push();
	    System.console().readLine();
	    supplier.disconnect_push_supplier();
	} 
	catch (Exception e)
	{ 
	    System.out.println("ATTENZIONE: Impossibile interagire con il consumer!\nMotivo: "+e.getCause());
	    System.exit(-1);
	}
    }
}
