import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;

public class PushConsumerImpl extends PushConsumerPOA
{
    ORB orb=null;
    int num=0;
    int somma=0;

    public PushConsumerImpl(ORB orb)
    {
	this.orb=orb;
    }

    public void push(org.omg.CORBA.Any data) throws Disconnected
    {
	System.out.println("Richiamato metodo push con data: "+data.extract_long()+" sul consumer");
	somma=somma+data.extract_long();
	num++;
    };

    public void disconnect_push_consumer()
    {
	System.out.println("Disconnesso consumer");
	if (num!=0)
	    System.out.println("Media: "+(double)somma/num);
	num=0;
	somma=0;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Ottengo il root naming context
	    org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
	    NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
	    //Faccio l'unbinding del riferimento al consumer nel naming
	    NameComponent path[] = ncRef.to_name("Consumer");
	    ncRef.unbind(path);
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	}
	catch(Exception e)
	{
	    System.out.println("ATTENZIONE: Impossibile gestire la disconnessione del PushConsumer!\nMotivo: "+e.getCause());
	}
    };
}