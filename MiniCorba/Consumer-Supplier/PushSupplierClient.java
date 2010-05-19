import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

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
	    PushConsumer consumer = PushConsumerHelper.narrow(objRef);
	    //Creo il servant
	    PushSupplierImpl supplier = new PushSupplierImpl();
	    int val=0;
	    System.out.println("Inserisci un valore, 0 per termimare");
	    boolean valido=false;
	    while (!valido)
		try
		{
		    val=Integer.parseInt(System.console().readLine());
		    valido=true;
		}
		catch(Exception e)
		{
		    System.out.println("ATTENZIONE: Inserire un valore numerico!");
		}
	    while(val!=0)
	    {
		consumer.push(val);
		System.out.println("Richiamato push dal client");
		valido=false;
		while (!valido)
		    try
		    {
			val=Integer.parseInt(System.console().readLine());
			valido=true;
		    }
		    catch(Exception e)
		    {
			System.out.println("ATTENZIONE: Inserire un valore numerico!");
		    }
	    }
	    consumer.disconnect_push_consumer();
	    System.out.println("Richiamato disconnetti dal client");
	} 
	catch (Exception e)
	{ 
	    System.out.println("ATTENZIONE: Impossibile interagire con il consumer!\nMotivo: "+e.getCause());
	    System.exit(-1);
	}
    }
}
