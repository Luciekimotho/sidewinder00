package distsys.bully.comm;

import javax.jms.JMSException;
import javax.jms.MessageFormatException;
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
	
	private volatile boolean abort = false;
	
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
	 * Starts the election procedure
	 */
	/*
	public void startElection(TopicPublisher publisher) {
		try {
			// Send election request
			TextMessage election = session.createTextMessage(Misc.ELECTION);
			election.setStringProperty(Misc.TYPE, Misc.ELECTION);
			election.setIntProperty(Misc.SOURCE, Bully.getClientID());
			setAbort(false);
			publisher.publish(election);
			// Wait for replies
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				Misc.error("An unrecoverable error happened while waiting for abort messages.", e);
			}
			if (!isAbort()) {
				// Nobody replied, I am the new coordinator
				Misc.writeLog("Election won!");
				// Send new coordinator ID
				publisher.publish(coordinatorMessage);
				// Set coordinator
				Bully.setCoordinator(Bully.getClientID());
				// Remove election inhibition
				Bully.setInhibitElection(false);
			} else {
				// TODO: Election lost. Is there something to be done?
				Misc.writeLog("Aborting election!");
			}
		} catch (MessageFormatException e) {
			Misc.error("Required property not found in the received message.", e);
		} catch (JMSException e) {
			Misc.error("An unrecoverable error happened.", e);
		}
	}
	/*
	
	/**
	 * Sends a ping and waits for the PONG
	 */
	// TODO: change the name of this method
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
					} else {
						//Misc.writeLog("Ping succeeded! (" + ping.getText() + ")");
					}
				}
			} catch(JMSException e) {
				Misc.error("An unrecoverable error happened.", e);
			}
		}
	}
	
	public boolean isAbort() {
		return abort;
	}
	
	public void setAbort(boolean abort) {
		this.abort = abort;
	}
	
	public Election getElection() {
		return election;
	}
}
