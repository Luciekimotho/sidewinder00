import Type.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

public class IImpl extends IPOA
{
    ORB orb=null;
    String string=null;

    public IImpl(ORB orb)
    {
	this.orb=orb;
    }

    public void pushString (String data) throws Disconnected
    {
	System.out.println("IImpl:\tRichiamato metodo push con data: "+data);
	if (string!=null)
	    string=string+" "+data;
	else
	    string=data;
    };

    public void disconnect_push_consumer()
    {
	System.out.println("IImpl:\tDisconnesso consumer");
	System.out.println(string);
	string=null;
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
	    System.out.println("IImpl:\tATTENZIONE: Impossibile gestire la distruzione del PushConsumer!\nMotivo: "+e.getCause());
	}
    };
}