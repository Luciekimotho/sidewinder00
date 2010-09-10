import CosTypedEventChannelAdmin.*;
import CosEventChannelAdmin.*;
import Type.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

public class Client 
{
    public static void main(String args[]) 
    {
	try
	{
	    //Creo e inizializzo l'ORB
	    ORB orb = ORB.init(args, null);
	    TypedConsumerAdmin typedConsumerAdmin=null;
	    TypedSupplierAdmin typedSupplierAdmin=null;
	    ProxyPushConsumer proxyPushConsumer=null;
	    TypedProxyPushConsumer typedProxyPushConsumer=null;
	    ProxyPushSupplier proxyPushSupplier=null;
	    ProxyPushSupplier typedProxyPushSupplier=null;
	    PushSupplierImpl push_supplier=null;
	    TypedPushConsumerImpl typed_push_consumer=null;
	    PushConsumerImpl push_consumer=null;
	    I i=null;
	    System.out.println("Digita:");
	    System.out.println("0: Uscire");	
	    System.out.println("1: Ottenere TypedConsumerAdmin");
	    System.out.println("2:     	Richiamare metodo obtain_push_supplier sul TypedConsumerAdmin");
	    System.out.println("3:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
	    System.out.println("4:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
	    System.out.println("5:    	Richiamare metodo obtain_typed_push_supplier sul TypedConsumerAdmin");	    
	    System.out.println("6:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
	    System.out.println("7:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
	    System.out.println("8: Ottenere TypedSupplierAdmin");
	    System.out.println("9:	Richiamare metodo obtain_push_consumer sul TypedSupplierAdmin");
	    System.out.println("10: 		Richiamare metodo connect_push_supplier sul ProxyPushConsumer");
	    System.out.println("11: 			Richiamare metodo push sul ProxyPushConsumer");
	    System.out.println("12: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
	    System.out.println("13:    	Richiamare metodo obtain_typed_push_consumer sul TypedSupplierAdmin");
	    System.out.println("14: 		Richiamare metodo connect_push_supplier sul TypedProxyPushConsumer");
	    System.out.println("15: 			Richiamare metodo push sul TypedProxyPushConsumer");
	    System.out.println("16: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
	    System.out.println("17: 		Richiamare metodo get_typed_consumer sul TypedProxyPushConsumer per ottenere l'interfaccia");
	    System.out.println("18: 			Richiamare metodo push sull'interfaccia tipata");
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
		    System.out.println("Client:\tATTENZIONE: Inserire un valore numerico!");
		}
	    while (scelta!=0)
	    {
		if (scelta==1)
		{
		    //Ottengo il riferimento all'oggetto dal naming
		    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedConsumerAdmin");
		    typedConsumerAdmin = TypedConsumerAdminHelper.narrow(objRef);
		    if (typedConsumerAdmin==null)
			System.out.println("Client:\tNessun TypedConsumerAdmin individuato!");
		    else
			System.out.println("Client:\tRestituito TypedConsumerAdmin: "+typedConsumerAdmin);
		}
		else if (scelta==2)
		{
		    if (typedConsumerAdmin==null)
			System.out.println("Client:\tNessun TypedConsumerAdmin individuato!");
		    else
		    {
			try
			{
			    proxyPushSupplier=typedConsumerAdmin.obtain_push_supplier();
			    System.out.println("Client:\tRichiamato metodo obtain_push_supplier di TypedConsumerAdmin\nRestituito ProxyPushSupplier: "+proxyPushSupplier);
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tTypedConsumerAdmin non esistente a causa della distruzione del canale!");
			    typedConsumerAdmin=null;
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
		    if (typedConsumerAdmin==null)
			System.out.println("Client:\tNessun TypedConsumerAdmin individuato!");
		    else
		    {
			try
			{
			    System.out.println("Client:\tInserisci la chiave:");
			    String chiave=System.console().readLine();
			    typedProxyPushSupplier=typedConsumerAdmin.obtain_typed_push_supplier(chiave);
			    System.out.println("Client:\tRichiamato metodo obtain_typed_push_supplier di TypedConsumerAdmin\nRestituito TypedProxyPushSupplier: "+typedProxyPushSupplier);
			}
			catch(NoSuchImplementation nsi)
			{
			    System.out.println("Client:\tTipo non supportato!");
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tTypedConsumerAdmin non esistente a causa della distruzione del canale!");
			    typedConsumerAdmin=null;
			}
		    }
		}
		else if (scelta==6)
		{
		    if (typedProxyPushSupplier==null)
			System.out.println("Client:\tNessun TypedProxyPushSupplier individuato!");
		    else
		    {
			try
			{
			    if (typed_push_consumer==null)
				typed_push_consumer=new TypedPushConsumerImpl(orb);
			    //Ottengo il riferimento al rootPOA e attivo il POAManager
			    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			    rootPOA.the_POAManager().activate();
			    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
			    org.omg.CORBA.Object ref = rootPOA.servant_to_reference(typed_push_consumer);
			    typedProxyPushSupplier.connect_push_consumer(PushConsumerHelper.narrow(ref));
			    System.out.println("Client:\tRichiamato metodo connect_push_consumer di TypedProxyPushSupplier");
			}
			catch(AlreadyConnected ac)
			{
			    System.out.println("Client:\tIl PushConsumer è gia connesso!");
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tTypedProxyPushSupplier non esistente a causa della distruzione del canale!");
			    typedProxyPushSupplier=null;
			}
		    }
		}
		else if (scelta==7)
		{
		    if (typed_push_consumer==null)
			System.out.println("Client:\tNessun PushConsumer individuato!");
		    else
		    {
			typed_push_consumer.disconnect_push_consumer();
			System.out.println("Client:\tRichiamato metodo disconnect_push_consumer di PushConsumer");
		    }
		}
		else if (scelta==8)
		{
		    org.omg.CORBA.Object objRef = orb.string_to_object("corbaname::localhost:1050#TypedSupplierAdmin");
		    typedSupplierAdmin = TypedSupplierAdminHelper.narrow(objRef);
		    if (typedSupplierAdmin==null)
			System.out.println("Client:\tNessun TypedSupplierAdmin individuato!");
		    else
			System.out.println("Client:\tRestituito TypedSupplierAdmin: "+typedSupplierAdmin);
		}
		else if (scelta==9)
		{
		    if (typedSupplierAdmin==null)
			System.out.println("Client:\tNessun TypedSupplierAdmin individuato!");
		    else
		    {
			try
			{
			    proxyPushConsumer=typedSupplierAdmin.obtain_push_consumer();
			    System.out.println("Client:\tRichiamato metodo obtain_push_consumer di TypedSupplierAdmin\nRestituito ProxyPushConsumer: "+proxyPushConsumer);
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tTypedSupplierAdmin non esistente a causa della distruzione del canale!");
			    typedSupplierAdmin=null;
			}
		    }
		}
		else if (scelta==10)
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
				    System.out.println("Client:\tATTENZIONE: Inserire un valore numerico!");
				}
			    if (scelta!=0 && scelta!=1)
				System.out.println("Client:\tScelta non valida!");
			}
			if (scelta==0)
			{
			    try
			    {
				if (push_supplier==null)
				    push_supplier=new PushSupplierImpl(orb,proxyPushConsumer);
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
		else if (scelta==11)
		{
		    if (proxyPushConsumer==null)
			System.out.println("Client:\tNessun ProxyPushConsumer individuato!");
		    else
		    {
			 System.out.println("Client:\tAvviato il generatore di push richiamando il metodo start_push");
			 push_supplier.start_push();
		    }
		}
		else if (scelta==12)
		{
		    if (push_supplier==null)
			System.out.println("Client:\tNessun PushSupplier individuato!");
		    else
		    {
			push_supplier.disconnect_push_supplier();
			System.out.println("Client:\tRichiamato metodo disconnect_push_supplier di PushSupplier");
		    }
		}
		else if (scelta==13)
		{
		    if (typedSupplierAdmin==null)
			System.out.println("Client:\tNessun TypedSupplierAdmin individuato!");
		    else
		    {
			try
			{
			    System.out.println("Client:\tInserisci la chiave:");
			    String chiave=System.console().readLine();
			    typedProxyPushConsumer=typedSupplierAdmin.obtain_typed_push_consumer(chiave);
			    System.out.println("Client:\tRichiamato metodo obtain_typed_push_consumer di TypedSupplierAdmin\nRestituito TypedProxyPushConsumer: "+typedProxyPushConsumer);
			}
			catch(InterfaceNotSupported ins)
			{
			    System.out.println("Client:\tTipo non supportato!");
			}
			catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			{
			    System.out.println("Client:\tTypedSupplierAdmin non esistente a causa della distruzione del canale!");
			    typedSupplierAdmin=null;
			}
		    }
		}
		else if (scelta==14)
		{
		    if (typedProxyPushConsumer==null)
			System.out.println("Client:\tNessun TypedProxyPushConsumer individuato!");
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
				    System.out.println("Client:\tATTENZIONE: Inserire un valore numerico!");
				}
			    if (scelta!=0 && scelta!=1)
				System.out.println("Client:\tScelta non valida!");
			}
			if (scelta==0)
			{
			    try
			    {
				if (push_supplier==null)
				    push_supplier=new PushSupplierImpl(orb,typedProxyPushConsumer);
				//Ottengo il riferimento al rootPOA e attivo il POAManager
				POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
				rootPOA.the_POAManager().activate();
				//Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
				org.omg.CORBA.Object ref = rootPOA.servant_to_reference(push_supplier);
				typedProxyPushConsumer.connect_push_supplier(PushSupplierHelper.narrow(ref));
				System.out.println("Client:\tRichiamato metodo connect_push_supplier di TypedProxyPushConsumer con avviso di disconnessione");
			    }
			    catch(AlreadyConnected ac)
			    {
				System.out.println("Client:\tIl PushSupplier è gia connesso!");
			    }
			    catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			    {
				System.out.println("Client:\tTypedProxyPushConsumer non esistente a causa della distruzione del canale!");
				typedProxyPushConsumer=null;
			    }
			}
			else
			{
			    try
			    {
				typedProxyPushConsumer.connect_push_supplier(null);
				System.out.println("Client:\tRichiamato metodo connect_push_supplier di TypedProxyPushConsumer senza avviso di disconnessione");
			    }
			    catch(org.omg.CORBA.OBJECT_NOT_EXIST one)
			    {
				System.out.println("Client:\tTypedProxyPushConsumer non esistente a causa della distruzione del canale!");
				typedProxyPushConsumer=null;
			    }
			}
		    }
		}
		else if (scelta==15)
		{
		    if (typedProxyPushConsumer==null)
			System.out.println("Client:\tNessun TypedProxyPushConsumer individuato!");
		    else
		    {
			 System.out.println("Client:\tAvviato il generatore di push richiamando il metodo start_push");
			 push_supplier.start_push();
		    }
		}
		else if (scelta==16)
		{
		    if (push_supplier==null)
			System.out.println("Client:\tNessun PushSupplier individuato!");
		    else
		    {
			push_supplier.disconnect_push_supplier();
			System.out.println("Client:\tRichiamato metodo disconnect_push_supplier di PushSupplier");
		    }
		}
		else if (scelta==17)
		{
		    if (typedProxyPushConsumer==null)
			System.out.println("Client:\tNessun TypedProxyPushConsumer individuato!");
		    else
		    {
			System.out.println("Client:\tRichiamato metodo get_typed_consumer di TypedProxyPushConsumer");
			if (i==null)
			    i=IHelper.narrow(typedProxyPushConsumer.get_typed_consumer());
		    }
		}
		else if (scelta==18)
		{
		    if (i==null)
			System.out.println("Client:\tNessun interfaccia tipata individuata!");
		    else
		    {
			System.out.println("Client:\tInserisci una stringa, \".\" per termimare");
			String string=System.console().readLine();
			while(!string.equals("."))
			{
			    i.pushString(string);
			    System.out.println("Client:\tRichiamato push sull'interfaccia tipata");
			    string=System.console().readLine();
			}
		    }
		}
		else
		    System.out.println("Client:\tScelta non valida!");
		System.out.println("");
		System.out.println("");
		System.out.println("Digita:");
		System.out.println("0: Uscire");	
		System.out.println("1: Ottenere TypedConsumerAdmin");
		System.out.println("2:     	Richiamare metodo obtain_push_supplier sul TypedConsumerAdmin");
		System.out.println("3:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
		System.out.println("4:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
		System.out.println("5:    	Richiamare metodo obtain_typed_push_supplier sul TypedConsumerAdmin");	    
		System.out.println("6:         	Richiamare metodo connect_push_consumer sul ProxyPushSupplier");
		System.out.println("7:         		Richiamare metodo disconnect_push_consumer sul PushConsumer");
		System.out.println("8: Ottenere TypedSupplierAdmin");
		System.out.println("9:	Richiamare metodo obtain_push_consumer sul TypedSupplierAdmin");
		System.out.println("10: 		Richiamare metodo connect_push_supplier sul ProxyPushConsumer");
		System.out.println("11: 			Richiamare metodo push sul ProxyPushConsumer");
		System.out.println("12: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
		System.out.println("13:    	Richiamare metodo obtain_typed_push_consumer sul TypedSupplierAdmin");
		System.out.println("14: 		Richiamare metodo connect_push_supplier sul TypedProxyPushConsumer");
		System.out.println("15: 			Richiamare metodo push sul TypedProxyPushConsumer");
		System.out.println("16: 			Richiamare metodo disconnect_push_supplier sul PushSupplier");
		System.out.println("17: 		Richiamare metodo get_typed_consumer sul TypedProxyPushConsumer per ottenere l'interfaccia");
		System.out.println("18: 			Richiamare metodo push sull'interfaccia tipata");
		valido=false;
		while (!valido)
		    try
		    {
			scelta=Integer.parseInt(System.console().readLine());
			valido=true;
		    }
		    catch(Exception e)
		    {
			System.out.println("Client:\tATTENZIONE: Inserire un valore numerico!");
		    }
	    }
	    System.out.println("Client:\tBye bye!");
	} 
	catch (Exception e)
	{ 
	    e.printStackTrace(); 
	}
    }
}
