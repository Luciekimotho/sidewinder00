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