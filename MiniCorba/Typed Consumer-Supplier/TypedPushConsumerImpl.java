import CosTypedEventComm.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;

public class TypedPushConsumerImpl extends TypedPushConsumerPOA
{
    int num=0;
    int somma=0;
    ORB orb=null;

    public TypedPushConsumerImpl(ORB orb)
    {
	this.orb=orb;
    }
    
    public org.omg.CORBA.Object get_typed_consumer()
    {
	org.omg.CORBA.Object ref=null;
	try
	{
	    //throw new org.omg.CORBA.BAD_INV_ORDER();
	    IImpl i=new IImpl();
	    //Ottengo il riferimento al rootPOA e attivo il POAManager
	    POA rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
	    rootPOA.the_POAManager().activate();
	    //Attivo il servant associandolo al rootPOA e ottenendo il riferimento all'oggetto
	    ref = rootPOA.servant_to_reference(i);
	}
	catch(Exception e)
	{
	    System.out.println("ATTENZIONE: Impossibile gestire la creazione dell'interfaccia tipata!\nMotivo: "+e.getCause());
	}
	return ref;
    };

    public void push (int data) throws Disconnected
    {
	//Potrei anche non implementarla se voglio usare solo typed e così lancia l'eccezzione NO_IMPLEMENT
	System.out.println("Richiamato metodo push con data: "+data);
	somma+=data;
	num++;
    };
    
    public void disconnect_push_consumer()
    {
	System.out.println("Disconnesso consumer");
	if (num!=0)
	    System.out.println("Media: "+(double)somma/num);
	num=0;
	somma=0;
    };
}