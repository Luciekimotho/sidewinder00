import CosTypedEventComm.*;
import Type.*;
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
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedConsumer");
	    TypedPushConsumer consumer = TypedPushConsumerHelper.narrow(objRef);
	    //Creo il servant
	    PushSupplierImpl supplier = new PushSupplierImpl();
	    int scelta=2;
	    while (scelta!=0 && scelta!=1)
	    {
		System.out.println("Digita:");
		System.out.println("0: Per avviare una comunicazione generica");
		System.out.println("1: Per avviare una comunicazione tipata");
		boolean valido=false;
		while (!valido)
		    try
		    {
			scelta=Integer.parseInt(System.console().readLine());
			valido=true;
		    }
		    catch(Exception e)
		    {
			System.out.println("ATTENZIONE: Inserire un valore numerico!");
		    }
		if (scelta!=0 && scelta!=1)
		    System.out.println("Scelta non valida!");
	    }
	    if (scelta==0)
	    {
		System.out.println("Inserisci un valore, 0 per termimare");
		boolean valido=false;
		while (!valido)
		    try
		    {
			scelta=Integer.parseInt(System.console().readLine());
			valido=true;
		    }
		    catch(Exception e)
		    {
			System.out.println("ATTENZIONE: Inserire un valore numerico!");
		    }
		while(scelta!=0)
		{
		    consumer.push(scelta);
		    System.out.println("Richiamato push dal client");
		    valido=false;
		    while (!valido)
			try
			{
			    scelta=Integer.parseInt(System.console().readLine());
			    valido=true;
			}
			catch(Exception e)
			{
			    System.out.println("ATTENZIONE: Inserire un valore numerico!");
			}
		}
	    }
	    else
	    {
		I i=null;
		try
		{
		    i=IHelper.narrow(consumer.get_typed_consumer());
		}
		catch(Exception e)
		{
		    System.out.println("ATTENZIONE: interfaccia differente da quella concordata!\nMotivo: "+e.getCause());
		    System.exit(-1);
		}
		System.out.println("Inserisci una stringa, end per termimare");
		String string=System.console().readLine();
		while(!string.equals("end"))
		{
		    i.push(string);
		    System.out.println("Richiamato push dal client");
		    string=System.console().readLine();
		}
		i.disconnect_push_consumer();
	    }
	    consumer.disconnect_push_consumer();
	    System.out.println("Richiamato disconnetti dal client");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
