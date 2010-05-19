import Type.*;

public class IImpl extends IPOA
{
    String string=null;
    
    public void push (String data) throws Disconnected
    {
	System.out.println("Richiamato metodo push con data: "+data);
	if (string!=null)
	    string=string+" "+data;
	else
	    string=data;
    };
    
    public void disconnect_push_consumer()
    {
	System.out.println("Disconnesso consumer");
	System.out.println(string);
	string=null;
    };
}