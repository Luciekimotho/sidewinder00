package distsys.bully;

import java.net.ConnectException;
import java.net.UnknownHostException;

import javax.jms.TopicConnectionFactory;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.objectweb.joram.client.jms.Topic;
import org.objectweb.joram.client.jms.admin.AdminException;
import org.objectweb.joram.client.jms.admin.AdminModule;
import org.objectweb.joram.client.jms.admin.User;
import org.objectweb.joram.client.jms.tcp.TopicTcpConnectionFactory;

public class JMSSetup {

	public static void main(String[] args) {
		
		System.out.println("Setting up administered objects...");
		
		try {
			// Connecting to Joram administration
			AdminModule.connect("root", "root", 60);
			
			// Topic creation and ACL setup
			Topic topic = Topic.create("Bully");
			topic.setFreeReading();
			topic.setFreeWriting();
			System.out.println("Topic Bully created!");
			
			User.create("anonymous", "anonymous");
			System.out.println("User anonymous created!");
			
			// Topic Connection Factory creation
			TopicConnectionFactory tcf = TopicTcpConnectionFactory.create("localhost", 16010);
			System.out.println("Topic connection factory created");
			
			// Object binding
			Context ctx = new InitialContext();
			ctx.bind("Bully", topic);
			ctx.bind("BullyConnectionFactory", tcf);
			ctx.close();
			System.out.println("Topic and Topic Connection Factory successfully bound!");
			
			// Closing connection
			AdminModule.disconnect();
			System.out.println("All objects created successfully!");
			
		} catch (ConnectException e) {
			e.printStackTrace();
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (AdminException e) {
			e.printStackTrace();
		} catch (NamingException e) {
			e.printStackTrace();
		}
	}
}
