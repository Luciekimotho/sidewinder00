import CosEventComm.*;
import org.omg.CORBA.*;

public class PushSupplierImpl extends PushSupplierPOA
{
    PushConsumer consumer=null;
    ThreadPush t=null;

    public PushSupplierImpl(PushConsumer consumer,Any data)
    {
	this.consumer=consumer;
	t=new ThreadPush(consumer,data);
    }

    public void disconnect_push_supplier()
    {
	System.out.println("Disconnesso supplier");
	t.attivo=false;
	consumer.disconnect_push_consumer();
    };

    //Aggiunto io per simulare delle push
    public void start_push()
    {
	System.out.println("Richiamato start_push sul supplier "+t);
	t.start();
    };
}