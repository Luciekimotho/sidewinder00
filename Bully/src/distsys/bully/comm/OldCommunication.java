package distsys.bully.comm;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.objectweb.joram.client.jms.TopicConnectionFactory;

import distsys.bully.misc.Misc;

public class OldCommunication {
	
	private int clientID;
	
	// Connection
	private TopicConnection connection;
	// Publisher
	private TopicSession publisherSession;
	private TopicPublisher publisher;
	private TextMessage message;
	// Asynchronous subscriber
	private TopicSession asyncSubscriberSession;
	private TopicSubscriber asyncSubscriber;
	// Ping subscriber
	private TopicSession pingSession;
	private TopicSubscriber pingSubscriber;
	// Election subscriber
	private TopicSession electionSession;
	private TopicSubscriber electionSubscriber;
	
	public static final String ELECTION = "ELECTION";
	public static final String OK = "OK";
	public static final String PING = "PING";
	public static final String PONG = "PONG";

	/**
	 * Constructor. Simply sets the clientID
	 * @param clientID id of the client
	 */
	public OldCommunication(int clientID) {
		this.clientID = clientID;
		// TODO: Move some init code from start() to here!!! start() should start the connections and set the messagelistener
	}
	
	/**
	 * Start connection with JMS
	 */
	public void start() {
		try {
			// Looking up resources
			Misc.writeLog("Looking up resources...");
			Context ictx;
			ictx = new InitialContext();
			Topic topic = (Topic)ictx.lookup("Bully");
			Misc.writeLog("Bully topic found");
			TopicConnectionFactory tcf = (TopicConnectionFactory)ictx.lookup("BullyConnectionFactory");
			Misc.writeLog("BullyConnectionFactory found!");
			ictx.close();
			
			// Creating JMS objects
			Misc.writeLog("Creating JMS objects...");
			connection = tcf.createTopicConnection("root", "root");
			// Publisher
			publisherSession = connection.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
			publisher = publisherSession.createPublisher(topic);
			// Message
			message = publisherSession.createTextMessage();
			message.setIntProperty("Sender", clientID);
			// Async Subscriber
			asyncSubscriberSession = connection.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
			// Ignoring our own messages and messages coming from clients with our same ID (this
			// can happen only in case of misconfigurations) and accepting only PING and ELECTION messages
			asyncSubscriber = asyncSubscriberSession.createSubscriber(topic, "Sender <> " + clientID + 
					" AND Type = '" + PING + "'" +
					" AND Type = '" + ELECTION + "'", true);
			asyncSubscriber.setMessageListener(new ElectionMessageListener());
			// Ping Subscriber
			pingSession = connection.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
			// Ignoring our own messages and messages coming from clients with our same ID (this
			// can happen only in case of misconfigurations) and accepting only PONG messages
			pingSubscriber = pingSession.createSubscriber(topic, "Sender <> " + clientID + 
					" AND Type = '" + PONG + "'", true);
			// Election Subscriber
			electionSession = connection.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
			// Ignoring our own messages and messages coming from clients with our same ID (this
			// can happen only in case of misconfigurations) and accepting only OK messages
			electionSubscriber = electionSession.createSubscriber(topic, "Sender <> " + clientID + 
					" AND Type = '" + OK + "'", true);
			
			// Start connection
			connection.start(); // TODO: start the other connections as well!
			System.out.println("Initialization complete!");
			
		} catch(NamingException e) {
			e.printStackTrace();
		} catch(JMSException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Sends a message to the topic. The message contains the Sender property
	 * set the the clientID
	 * @param content content of the message
	 */
	// TODO: is it still needed?
	public synchronized void sendMessage(String content, String type) {
		try {
			message.setText(content);
			message.setStringProperty("Type", type);
			publisher.publish(message);
			publisherSession.commit();
		} catch (JMSException e) {
			e.printStackTrace();
		}
	}
	
	// TODO: Check and test!
	public synchronized boolean ping(int client) {
		Message tmpMessage;
		
		try {
			// Sending the PING message
			message.setText("Ping");
			message.setStringProperty("Type", PING);
			publisher.publish(message);
			publisherSession.commit();
			
			// Waiting for the PONG or for the timeout
			do {
				tmpMessage = pingSubscriber.receive(200); // pingSubscriber only receives messages with Type = PING
				// If timeout expired, return null
				if (tmpMessage == null) {
					return false;
				}
			} while(tmpMessage.getIntProperty("Sender") != client);
			
			// PING succeeded if a PONG is received from the client we pinged
			return true;
			
		} catch (JMSException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	// TODO: to implement!
	public synchronized boolean election() {
		return false;
	}
	
	/**
	 * Terminate the connection
	 */
	public void stop() {
		try {
			connection.close(); // TODO: stop the other connections as well!
			System.out.println("JMS connections closed successfully");
		} catch (JMSException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Message listener 
	 */
	class ElectionMessageListener implements MessageListener {

		@Override
		public void onMessage(Message message) {
			// Check if the message is a TextMessage
			if (message instanceof TextMessage) {
				TextMessage textMessage = (TextMessage)message;
				try {
					if (textMessage.getStringProperty("Type").equals(PING)) {
						sendMessage("", PONG); // Maybe a sendPing() would be better!
					}
					System.out.println(textMessage.getText());
				} catch (JMSException e) {
					e.printStackTrace();
				}
			}
		}
	}
}
