import CosEventComm.*;
import org.omg.CORBA.*;

//Questo push consumer generico viene implementato in modo tale da inoltrare le richieste all'implementazione dell'interfaccia I
public class TypedPushConsumerImpl extends PushConsumerPOA
{
    IImpl i=null;

    public TypedPushConsumerImpl(ORB orb)
    {
	//Istanzio l'implementazione dell'interfaccia
	i=new IImpl(orb);
    }

    public void push(org.omg.CORBA.Any data) throws Disconnected
    {
	//Richiamo i metodi offerti dall'interfaccia
	i.pushString(data.extract_string());
    };

    public void disconnect_push_consumer()
    {
	//Richiamo i metodi offerti dall'interfaccia
	i.disconnect_push_consumer();
    };
}