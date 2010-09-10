import CosTypedEventChannelAdmin.*;
import org.omg.CORBA.*;

public class Admin 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo l'ORB
	    ORB orb = ORB.init(args, null);
	    //Ottengo il riferimento del typedEventChannel dal naming
	    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedEventChannel");
	    TypedEventChannel typedEventChannel = TypedEventChannelHelper.narrow(objRef);
	    if (typedEventChannel==null)
	    {
		System.out.println("Admin:\tNessun TypedEventChannnel individuato!");
		System.exit(-1);
	    }
	    System.out.println("Digita:");
	    System.out.println("0: Uscire");	
	    System.out.println("1: Richiamare metodo for_consumers");
	    System.out.println("2: Richiamare metodo for_suppliers");
	    System.out.println("3: Richiamare metodo destroy");
	    boolean valido=false;
	    int scelta=0;
	    while (!valido)
		try
		{
		    scelta=Integer.parseInt(System.console().readLine());
		    valido=true;
		}
		catch(Exception e)
		{
		    System.out.println("Admin:\tATTENZIONE: Inserire un valore numerico!");
		}
	    while (scelta!=0)
	    {
		if (scelta==1)
		{
		    TypedConsumerAdmin typedConsumerAdmin=typedEventChannel.for_consumers();
		    System.out.println("Admin:\tRichiamato metodo for_consumers di TypedEventChannel.\nRestituito TypedConsumerAdmin: "+typedConsumerAdmin);
		}
		else if (scelta==2)
		{
		    TypedSupplierAdmin typedSupplierAdmin=typedEventChannel.for_suppliers();
		    System.out.println("Admin:\tRichiamato metodo for_suppliers di TypedEventChannel.\nRestituito TypedSupplierAdmin: "+typedSupplierAdmin);
		}
		else if (scelta==3)
		{
		    typedEventChannel.destroy();
		    System.out.println("Admin:\tRichiamato metodo destroy di TypedEventChannel");
		    break;
		}
		else
		    System.out.println("Admin:\tScelta non valida!");
		System.out.println("");
		System.out.println("");
		System.out.println("Digita:");
		System.out.println("0: Uscire");	
		System.out.println("1: Richiamare metodo for_consumers");
		System.out.println("2: Richiamato metodo for_suppliers");
		System.out.println("3: Richiamare metodo destroy");
		valido=false;
		while (!valido)
		    try
		    {
			scelta=Integer.parseInt(System.console().readLine());
			valido=true;
		    }
		    catch(Exception e)
		    {
			System.out.println("Admin:\tATTENZIONE: Inserire un valore numerico!");
		    }
	    }
	    System.out.println("Admin:\tBye bye!");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
 
