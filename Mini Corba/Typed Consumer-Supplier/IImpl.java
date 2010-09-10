import CosEventComm.*;
import Type.*;

public class IImpl extends IPOA
{
    String string=null;
    boolean connected=true;
    
    public void push (String data) throws Disconnected
    {
	if (connected==false)
	    throw new org.omg.CORBA.BAD_INV_ORDER();
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

    //NOTE Aggiunto da me per notificare all'utente che sta usando una comunicazione tipata che il consumer è disconnesso
    public void change_state()
    {
	connected=false;
    };
}