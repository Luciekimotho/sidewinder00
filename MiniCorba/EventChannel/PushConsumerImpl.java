import CosEventComm.*;

public class PushConsumerImpl extends PushConsumerPOA
{
    int num=0;
    int somma=0;
    /*public void push (org.omg.CORBA.Any data) throws Disconnected
    {
	System.out.println("Richiamato metodo push con data: "+data);
    };*/

    public void push (int data) throws Disconnected
    {
	System.out.println("PushConsumerImpl:\tRichiamato metodo push con data: "+data);
	somma+=data;
	num++;
    };
    
    public void disconnect_push_consumer()
    {
	System.out.println("PushConsumerImpl:\tDisconnesso consumer");
	if (num!=0)
	    System.out.println("PushConsumerImpl:\tMedia: "+(double)somma/num);
	num=0;
	somma=0;
    };
}