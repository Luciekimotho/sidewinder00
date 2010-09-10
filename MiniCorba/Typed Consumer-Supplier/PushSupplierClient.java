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
	    //Ottengo il riferimento al TypedConsumer dal naming
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedPushConsumer");
	    if (objRef==null)
	    {
		System.out.println("ATTENZIONE: Nessun TypedPushConsumer individuato!");
		System.exit(-1);
	    }
	    TypedPushConsumer consumer = TypedPushConsumerHelper.narrow(objRef);
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
		//Creo il servant
		Any data=orb.create_any();
		PushSupplierImpl supplier = new PushSupplierImpl(consumer,data);
		System.out.println("Premi invio per avviare il generatore di push e ripremilo per disconnettere il supplier");
		System.console().readLine();
		supplier.start_push();
		System.console().readLine();
		supplier.disconnect_push_supplier();
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
		//Creo il servant
		PushSupplierImpl supplier = new PushSupplierImpl(consumer,i);
		System.out.println("Inserisci una stringa, \".\" per termimare");
		String string=System.console().readLine();
		while(!string.equals("."))
		{
		    try
		    {
			i.push(string);
		    }
		    catch(org.omg.CORBA.BAD_INV_ORDER bio)
		    {
			System.out.println("ATTENZIONE: Il consumer si è disconnesso, impossibile continuare una comunicazione tipata!");
			System.exit(-1);
		    }
		    System.out.println("Richiamato push dal client");
		    string=System.console().readLine();
		}
		supplier.disconnect_push_supplier();
	    }
	    System.out.println("Richiamato disconnetti dal client");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
