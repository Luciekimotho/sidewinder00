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
	System.out.println("PushConsumerImpl:\tRichiamato metodo push con data: "+data.extract_long()+" sul consumer");
	somma=somma+data.extract_long();
	num++;
    };

    public void disconnect_push_consumer()
    {
	System.out.println("PushConsumerImpl:\tDisconnesso consumer");
	if (num!=0)
	    System.out.println("Media: "+(double)somma/num);
	num=0;
	somma=0;
	try
	{
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Disattivo nel rootPOA il PushConsumer
	    byte[] id=rootPOA.servant_to_id(this);
	    rootPOA.deactivate_object(id);
	}
	catch(Exception e)
	{
	    System.out.println("PushConsumerImpl:\tATTENZIONE: Impossibile gestire la distruzione del PushConsumer!\nMotivo: "+e.getCause());
	}
    };
}