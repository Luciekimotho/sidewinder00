package distsys.bully.comm;

import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import distsys.bully.Bully;
import distsys.bully.misc.Misc;

public class BullyEngine implements Runnable {

	private volatile boolean running = false;
		
	private TopicSession session;
	private TopicPublisher publisher;
	private TopicSubscriber pingSubscriber;
	
	private TextMessage coordinatorMessage;
	private TextMessage pingMessage;
	
	private Election election;
	
	public BullyEngine(TopicConnection connection, Topic topic) {
		try {
			this.session = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
			publisher = session.createPublisher(topic);
			pingSubscriber = session.createSubscriber(topic, Misc.TYPE + " = '" + Misc.PONG + 
									"' AND " + Misc.DESTINATION + " = " + Bully.getClientID(), true);
			
			coordinatorMessage = session.createTextMessage();
			coordinatorMessage.setStringProperty(Misc.TYPE, Misc.COORDINATOR);
			coordinatorMessage.setIntProperty(Misc.COORDINATOR, Bully.getClientID());
			
			pingMessage = session.createTextMessage();
			pingMessage.setStringProperty(Misc.TYPE, Misc.PING);
			pingMessage.setIntProperty(Misc.SOURCE, Bully.getClientID());
			
			// Creating a new session for the Election thread (session is a single threaded object)
			election = new Election(connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE), topic);
		} catch(JMSException e) {
			Misc.error("An unrecoverable error happened.", e);
		}
	}
	
	/**
	 * Closes the session used to send ping messages
	 * @throws JMSException if the session throws errors when closing
	 */
	public void exit(){
		running = false;
	}
	
	public void run() {
		running = true;
		
		// Initiating startup election
		synchronized (this) {
			Misc.writeLog("Initiating startup election");
			Bully.setInhibitElection(true);
			//startElection(publisher);
			new Thread(election).start();
		}
		
		// Main cycle
		while(running) {
			ping();
			try {
				// Timeout between pings
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				running = false;
			}
		}
		
		try {
			session.close();
		} catch (JMSException e) {
			// Should be safe even if we get here. After all, we're shutting down the thread.
			e.printStackTrace();
		}
		Misc.writeLog("BullyEngine thread terminated");
	}
	
	/**
	 * Sends a ping and waits for the PONG
	 */
	private void ping() {
		synchronized(this) {
			try {
				// Send a test ping
				if (Bully.getClientID() != Bully.getCoordinator() && Bully.getCoordinator() != 0 && Bully.getInhibitElection() == false) {
					//Misc.writeLog("Sending a ping to the coordinator (client " + Bully.getCoordinator() + ")");
					pingMessage.setIntProperty(Misc.DESTINATION, Bully.getCoordinator());
					publisher.publish(pingMessage);
					TextMessage reply = null;
					reply = (TextMessage)pingSubscriber.receive(100);
					if (reply == null) {
						Misc.writeLog("Ping failed! Starting an election");
						// Resetting clientID and starting a new election
						Bully.setCoordinator(0);
						if (Bully.getInhibitElection() == false) {
							Bully.setInhibitElection(true);
							// startElection(publisher);
							new Thread(election).start();
						}
					}
				}
			} catch(JMSException e) {
				Misc.error("An unrecoverable error happened.", e);
			}
		}
	}
	
	public Election getElection() {
		return election;
	}
}
