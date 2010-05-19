import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class Admin 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo l'ORB
	    ORB orb = ORB.init(args, null);
	    //Ottengo il riferimento all'oggetto dal naming
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#EventChannel");
	    EventChannel eventChannel = EventChannelHelper.narrow(objRef);
	    if (eventChannel==null)
	    {
		System.out.println("Nessun EventChannnel individuato!");
		System.exit(-1);
	    }
	    System.out.println("Digita:");
	    System.out.println("0: Uscire");	
	    System.out.println("1: Richiamare metodo for_consumers");
	    System.out.println("2: Richiamare metodo for_suppliers");
	    System.out.println("3: Richiamare metodo destroy");
	    int scelta=Integer.parseInt(System.console().readLine());
	    while (scelta!=0)
	    {
		if (scelta==1)
		{
		    ConsumerAdmin consumerAdmin=eventChannel.for_consumers();
		    System.out.println("Admin:\tRichiamato metodo for_consumers di EventChannel.\nRestituito ConsumerAdmin: "+consumerAdmin);
		}
		else if (scelta==2)
		{
		    SupplierAdmin supplierAdmin=eventChannel.for_suppliers();
		    System.out.println("Admin:\tRichiamato metodo for_suppliers di EventChannel.\nRestituito SupplierAdmin: "+supplierAdmin);
		}
		else if (scelta==3)
		{
		    eventChannel.destroy();
		    System.out.println("Admin:\tRichiamato metodo destroy di EventChannel");
		    break;
		}
		else
		    System.out.println("Scelta non valida!");
		System.out.println("");
		System.out.println("");
		System.out.println("Digita:");
		System.out.println("0: Uscire");	
		System.out.println("1: Richiamare metodo for_consumers");
		System.out.println("2: Richiamato metodo for_suppliers");
		System.out.println("3: Richiamare metodo destroy");
		scelta=Integer.parseInt(System.console().readLine());
	    }
	    System.out.println("Bye bye!");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
 
