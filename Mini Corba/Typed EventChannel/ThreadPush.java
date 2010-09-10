import CosEventComm.*;
import CosEventChannelAdmin.*;
import org.omg.CORBA.*;
import java.util.Random;

public class ThreadPush extends Thread
{
    ProxyPushConsumer consumer=null;
    Any data=null;
    static boolean attivo=true;

    public ThreadPush(ProxyPushConsumer consumer,Any data)
    {
	//Mantengo il riferimento al ProxyPushConsumer che andrà poi a gestire l'inoltro dei dati che io invio a lui ai vari customer generici
	this.consumer=consumer;
	this.data=data;
    }

    public void run()
    {
	System.out.println("ThreadPush:\tAvviato thread...");
	Random random=new Random();
	while(attivo)
	{
	    data.insert_long(random.nextInt(21));
	    try
	    {
		consumer.push(data);
		System.out.println("ThreadPush:\tInoltrato "+data.extract_long()+" al consumer");
	    }
	    catch(Disconnected d)
	    {
		System.out.println("ThreadPush:\tATTENZIONE: Un consumer si è disconnesso!");
	    }
	    try
	    {
		Thread.sleep(3000);
	    }
	    catch(Exception e)
	    {
	    }
	}
    };
}