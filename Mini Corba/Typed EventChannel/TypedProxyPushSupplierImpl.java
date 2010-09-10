import CosEventChannelAdmin.*;
import CosEventComm.*;
import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.CosNaming.*;
import java.util.*;

public class TypedProxyPushSupplierImpl extends ProxyPushSupplierPOA
{
    ORB orb=null;
    List<PushConsumer> listPushConsumer=null;

    public TypedProxyPushSupplierImpl(ORB orb)
    {
	this.orb=orb;
	listPushConsumer=new ArrayList<PushConsumer>();
    }

    public void connect_push_consumer(PushConsumer push_consumer) throws AlreadyConnected, TypeError
    {
	System.out.println("TypedProxyPushSupplierImpl:\tRichiamato metodo connect_push_consumer con push_consumer: "+push_consumer);
	//Verifico che i parametri siano corretti e che il push consumer non sia gia presente nella lista, rilanciando l'eccezzione relativa se qualcosa non è corretto
	if (push_consumer==null)
	    throw new org.omg.CORBA.BAD_PARAM();
	else if (listPushConsumer.contains(push_consumer))
	    throw new AlreadyConnected();
	//Aggiungo il consumer alla lista per disconnetterlo successivamente
	listPushConsumer.add(push_consumer);
    };

    public void disconnect_push_supplier()
    {
	System.out.println("TypedProxyPushSupplierImpl:\tDisconnesso TypedProxyPushSupplier");
	//Per ogni PushConsumer tipizzato collegato al TypedProxyPushSupplier (e quindi presente nella lista) chiamo il metodo disconnect_push_consumer per disconnetterlo
	for(PushConsumer pushConsumer:listPushConsumer)
	    pushConsumer.disconnect_push_consumer();
	//Rimuovo tutti gli elementi dalla lista
	listPushConsumer.clear();
    };

    //NOTE Aggiunto per inoltrare i nuovi dati a tutti i consumer
    public void send_data(Any data) throws Disconnected
    {
	System.out.println("TypedProxyPushSupplierImpl:\tRichiamato metodo send_data con data: "+data);
	//Per ogni PushConsumer collegato richiamo il metodo push con il dato inoltrato dal supplier
	for(PushConsumer pushConsumer:listPushConsumer)
	    try
	    {
		pushConsumer.push(data);
	    }
	    catch(Exception e)
	    {
		System.out.println("TypedProxyPushSupplierImpl:\tATTENZIONE: PushConsumer "+pushConsumer+" disconnesso, lo rimuovo dalla lista!");
		listPushConsumer.remove(pushConsumer);
		//Rilancio l'eccezzione in modo da avvertire il client che qualche consumer non ha ricevuto l'info
		throw new Disconnected();
	    }
    };
}