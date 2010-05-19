import CosEventChannelAdmin.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;
import java.util.*;

public class ProxyPushSupplierImpl extends ProxyPushSupplierPOA
{
    ORB orb=null;
    List<PushConsumer> listPushConsumer=null;

    public ProxyPushSupplierImpl(ORB orb)
    {
	this.orb=orb;
	listPushConsumer=new ArrayList<PushConsumer>();
    }

    public void connect_push_consumer(PushConsumer push_consumer) throws AlreadyConnected, TypeError
    {
	//TODO Verifica il tipo se non va bene lancia TypeError
	if (listPushConsumer.contains(push_consumer))
	    throw new AlreadyConnected();
	else if (push_consumer==null)
	    throw new org.omg.CORBA.BAD_PARAM();
	listPushConsumer.add(push_consumer);
	System.out.println("ProxyPushSupplierImpl:\tRichiamato metodo connect_push_consumer con push_consumer: "+push_consumer);
    };

    public void disconnect_push_supplier()
    {
	System.out.println("ProxyPushSupplierImpl:\tDisconnesso ProxyPushSupplier");
	for(PushConsumer pushConsumer:listPushConsumer)
	    pushConsumer.disconnect_push_consumer();
	listPushConsumer.clear();
	//TODO Disconnettilo dal ConsumerAdmin??
    };

    //TODO Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public void send_data(int data) throws Disconnected
    {
	System.out.println("ProxyPushSupplierImpl:\tRichiamato metodo send_data con data: "+data);
	for(PushConsumer pushConsumer:listPushConsumer)
	    try
	    {
		pushConsumer.push(data);
	    }
	    catch(Disconnected d)
	    {
		System.out.println("ProxyPushSupplierImpl:\tATTENZIONE: PushConsumer "+pushConsumer+" disconnesso, lo rimuovo dalla lista!");
		listPushConsumer.remove(pushConsumer);
		//Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		throw d;
	    }
    };
}