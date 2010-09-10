import CosEventComm.*;
import org.omg.CORBA.*;
import java.util.Random;

public class ThreadPush extends Thread
{
    PushConsumer consumer=null;
    Any data=null;
    static boolean attivo=true;

    public ThreadPush(PushConsumer consumer,Any data)
    {
	this.consumer=consumer;
	this.data=data;
    }

    public void run()
    {
	System.out.println("Avviato thread...");
	Random random=new Random();
	while(attivo)
	{
	    data.insert_long(random.nextInt(21));
	    try
	    {
		consumer.push(data);
		System.out.println("Inoltrato "+data.extract_long()+" al consumer");
	    }
	    catch(Disconnected d)
	    {
		System.out.println("ATTENZIONE: Un consumer si è disconnesso!");
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