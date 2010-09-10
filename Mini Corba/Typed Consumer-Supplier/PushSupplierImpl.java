import Type.*;
import CosEventComm.*;
import CosTypedEventComm.*;
import org.omg.CORBA.*;

public class PushSupplierImpl extends PushSupplierPOA
{
    TypedPushConsumer consumer=null;
    ThreadPush t=null;
    I i=null;

    public PushSupplierImpl(TypedPushConsumer consumer,Any data)
    {
	this.consumer=consumer;
	t=new ThreadPush(consumer,data);
    }

    public PushSupplierImpl(TypedPushConsumer consumer,I i)
    {
	this.consumer=consumer;
	this.i=i;
    }

    public void disconnect_push_supplier()
    {
	System.out.println("Disconnesso supplier");
	if (t!=null)
	    t.attivo=false;
	else
	    i.disconnect_push_consumer();
	consumer.disconnect_push_consumer();
    };

    //Aggiunto io per simulare delle push
    public void start_push()
    {
	System.out.println("Richiamato start_push sul supplier "+t);
	t.start();
    };
}