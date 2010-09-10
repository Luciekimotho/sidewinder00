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
	System.out.println("ProxyPushSupplierImpl:\tRichiamato metodo connect_push_consumer con push_consumer: "+push_consumer);
	//Verifico se i parametri passati sono validi e che il push consumer non è gia presente nella lista, altrimenti lancio l'eccezzione relativa
	if (push_consumer==null)
	    throw new org.omg.CORBA.BAD_PARAM();
	else if (listPushConsumer.contains(push_consumer))
	    throw new AlreadyConnected();
	listPushConsumer.add(push_consumer);
    };

    public void disconnect_push_supplier()
    {
	System.out.println("ProxyPushSupplierImpl:\tDisconnesso ProxyPushSupplier");
	//Per ogni PushConsumer collegato al ProxyPushSupplier (e quindi presente nella lista) chiamo il metodo disconnect_push_consumer per disconnetterlo
	for(PushConsumer pushConsumer:listPushConsumer)
	    pushConsumer.disconnect_push_consumer();
	//Rimuovo tutti gli elementi dalla lista
	listPushConsumer.clear();
    };

    //NOTE Aggiunto per inoltrare i nuovi dati generici a tutti i consumer generici collegati a questo proxy
    public void send_data(Any data) throws Disconnected
    {
	System.out.println("ProxyPushSupplierImpl:\tRichiamato metodo send_data con data: "+data);
	//Per ogni PushConsumer collegato richiamo il metodo push con il dato inoltrato dal supplier
	for(PushConsumer pushConsumer:listPushConsumer)
	    try
	    {
		pushConsumer.push(data);
	    }
	    catch(Exception e)
	    {
		System.out.println("ProxyPushSupplierImpl:\tATTENZIONE: PushConsumer "+pushConsumer+" disconnesso, lo rimuovo dalla lista!");
		listPushConsumer.remove(pushConsumer);
		//Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		throw new Disconnected();
	    }
    };
}