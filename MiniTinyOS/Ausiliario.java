import java.io.*;

public class Ausiliario
{
    public static void main(String args[])
    {
	String path = "linkgain.out";
	String path2 = "linkgain2.out";
	char c;
	try 
	{
	    FileReader fr = new FileReader(path);
	    FileWriter fw = new FileWriter(path2);
	    int i;
	    i=fr.read();
	    while (i!=-1)
	    {
		c=(char)i;
		if (c==',')
		    fw.write('.');
		else
		    fw.write(c);
		i=fr.read();
	    }
	    fr.close();
	    fw.close();
	    File f1 = new File(path);
	    f1.delete();
	    File f2 = new File(path2);
	    f2.renameTo(f1);
	}
	catch(IOException e) 
	{
	  e.printStackTrace();
	}
    } 
}