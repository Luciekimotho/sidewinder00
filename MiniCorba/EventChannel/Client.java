import CosEventChannelAdmin.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class Client 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo l'ORB
	    ORB orb = ORB.init(args, null);
	    Any data=orb.create_any();
	    ConsumerAdmin consumerAdmin=null;
	    SupplierAdmin supplierAdmin=null;
	    ProxyPushConsumer proxyPushConsumer=null;
	    ProxyPushSupplier proxyPushSupplier=null;
	    PushSupplierImpl push_supplier=null;
	    PushConsumerImpl push_consumer=null;
	    System.out.println("Digita:");
	    System.out.println("0: Uscire");	
	    System.out.println("1: Ottenere ConsumerAdmin");
	    System.out.println("2:     	Richiamare metodo obtain_push_supplier sul ConsumerAdmin");
	    System.out.println("3:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
	    System.out.println("4:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
	    System.out.println("5: Ottenere SupplierAdmin");
	    System.out.println("6:	Richiamare metodo obtain_push_consumer sul SupplierAdmin");
	    System.out.println("7: 		Richiamare metodo connect_push_supplier sul ProxyPushConsumer");
	    System.out.println("8: 			Richiamare metodo push sul ProxyPushConsumer");
	    System.out.println("9: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
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
		    System.out.println("ATTENZIONE: Inserire un valore numerico!");
		}
	    while (scelta!=0)
	    {
		if (scelta==1)
		{
		    //Ottengo il riferimento all'oggetto dal naming
		    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#ConsumerAdmin");
		    consumerAdmin = ConsumerAdminHelper.narrow(objRef);
		    if (consumerAdmin==null)
			System.out.println("Client:\tNessun ConsumerAdmin individuato!");
		    else
			System.out.println("Client:\tRestituito ConsumerAdmin: "+consumerAdmin);
		}
		else if (scelta==2)
		{
		    if (consumerAdmin==null)
			System.out.println("Client:\tNessun ConsumerAdmin individuato!");
		    else
		    {
			try
			{
			    proxyPushSupplier=consumerAdmin.obtain_push_supplier();
			    System.out.println("Client:\tRichiamato metodo obtain_push_supplier di ConsumerAdmin\nRestituito ProxyPushSupplier: "+proxyPushSupplier);
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tConsumerAdmin non esistente a causa della distruzione del canale!");
			    consumerAdmin=null;
			}
		    }
		}
		else if (scelta==3)
		{
		    if (proxyPushSupplier==null)
			System.out.println("Client:\tNessun ProxyPushSupplier individuato!");
		    else
		    {
			try
			{
			    if (push_consumer==null)
				push_consumer=new PushConsumerImpl(orb);
			    //Ottengo il riferimento al rootPOA e attivo il POAManager
			    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			    rootPOA.the_POAManager().activate();
			    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
			    org.omg.CORBA.Object ref = rootPOA.servant_to_reference(push_consumer);
			    proxyPushSupplier.connect_push_consumer(PushConsumerHelper.narrow(ref));
			    System.out.println("Client:\tRichiamato metodo connect_push_consumer di ProxyPushSupplier");
			}
			catch(AlreadyConnected ac)
			{
			    System.out.println("Client:\tIl PushConsumer è gia connesso!");
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tProxyPushSupplier non esistente a causa della distruzione del canale!");
			    proxyPushSupplier=null;
			}
		    }
		}
		else if (scelta==4)
		{
		    if (push_consumer==null)
			System.out.println("Client:\tNessun PushConsumer individuato!");
		    else
		    {
			push_consumer.disconnect_push_consumer();
			System.out.println("Client:\tRichiamato metodo disconnect_push_consumer di PushConsumer");
		    }
		}
		else if (scelta==5)
		{
		    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#SupplierAdmin");
		    supplierAdmin = SupplierAdminHelper.narrow(objRef);
		    if (supplierAdmin==null)
			System.out.println("Client:\tNessun SupplierAdmin individuato!");
		    else
			System.out.println("Client:\tRestituito SupplierAdmin: "+supplierAdmin);
		}
		else if (scelta==6)
		{
		    if (supplierAdmin==null)
			System.out.println("Client:\tNessun SupplierAdmin individuato!");
		    else
		    {
			try
			{
			    proxyPushConsumer=supplierAdmin.obtain_push_consumer();
			    System.out.println("Client:\tRichiamato metodo obtain_push_consumer di SupplierAdmin\nRestituito ProxyPushConsumer: "+proxyPushConsumer);
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tSupplierAdmin non esistente a causa della distruzione del canale!");
			    supplierAdmin=null;
			}
		    }
		}
		else if (scelta==7)
		{
		    if (proxyPushConsumer==null)
			System.out.println("Client:\tNessun ProxyPushConsumer individuato!");
		    else
		    {
			while (scelta!=0 && scelta!=1)
			{
			    System.out.println("Digita:");
			    System.out.println("0: Per essere informato della disconnessione");
			    System.out.println("1: Per non essere informato della disconnessione");
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
			    if (scelta!=0 && scelta!=1)
				System.out.println("Scelta non valida!");
			}
			if (scelta==0)
			{
			    try
			    {
				if (push_supplier==null)
				    push_supplier=new PushSupplierImpl(orb,proxyPushConsumer,data);
				//Ottengo il riferimento al rootPOA e attivo il POAManager
				POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
				rootPOA.the_POAManager().activate();
				//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
				org.omg.CORBA.Object ref = rootPOA.servant_to_reference(push_supplier);
				proxyPushConsumer.connect_push_supplier(PushSupplierHelper.narrow(ref));
				System.out.println("Client:\tRichiamato metodo connect_push_supplier di ProxyPushConsumer con avviso di disconnessione");
			    }
			    catch(AlreadyConnected ac)
			    {
				System.out.println("Client:\tIl PushSupplier è gia connesso!");
			    }
			    catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			    {
				System.out.println("Client:\tProxyPushConsumer non esistente a causa della distruzione del canale!");
				proxyPushConsumer=null;
			    }
			}
			else
			{
			    try
			    {
				proxyPushConsumer.connect_push_supplier(null);
				System.out.println("Client:\tRichiamato metodo connect_push_supplier di ProxyPushConsumer senza avviso di disconnessione");
			    }
			    catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			    {
				System.out.println("Client:\tProxyPushConsumer non esistente a causa della distruzione del canale!");
				proxyPushConsumer=null;
			    }
			}
		    }
		}
		else if (scelta==8)
		{
		    if (proxyPushConsumer==null)
			System.out.println("Client:\tNessun ProxyPushConsumer individuato!");
		    else
		    {
			 System.out.println("Avviato il generatore di push");
			 push_supplier.start_push();
		    }
		}
		else if (scelta==9)
		{
		    if (push_supplier==null)
			System.out.println("Client:\tNessun PushSupplier individuato!");
		    else
		    {
			push_supplier.disconnect_push_supplier();
			System.out.println("Client:\tRichiamato metodo disconnect_push_supplier di PushSupplier");
		    }
		}
		else
		    System.out.println("Scelta non valida!");
		System.out.println("");
		System.out.println("");
		System.out.println("Digita:");
		System.out.println("0: Uscire");	
		System.out.println("1: Ottenere ConsumerAdmin");
		System.out.println("2:     	Richiamare metodo obtain_push_supplier sul ConsumerAdmin");
		System.out.println("3:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
		System.out.println("4:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
		System.out.println("5: Ottenere SupplierAdmin");
		System.out.println("6:	Richiamare metodo obtain_push_consumer sul SupplierAdmin");
		System.out.println("7: 		Richiamare metodo connect_push_supplier sul ProxyPushConsumer");
		System.out.println("8: 			Richiamare metodo push sul ProxyPushConsumer");
		System.out.println("9: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
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
	    System.out.println("Bye bye!");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
