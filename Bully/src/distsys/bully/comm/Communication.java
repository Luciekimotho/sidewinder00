package distsys.bully.comm;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import distsys.bully.Bully;
import distsys.bully.misc.Misc;

public class Communication {
	
	private TopicConnection connection;
	
	private Topic topic;
	private TopicConnectionFactory tcf;
	
	private TopicSession pongSession;
	private TopicSubscriber pongSubscriber;
	
	private TopicSession communicationSession;
	private TopicSession abortSession;
	private TopicPublisher electionPublisher;
	private TopicSubscriber abortSubscriber;
	private TopicSubscriber coordinatorSubscriber;
	private TopicSubscriber electionReceiverSubscriber;
	
	private TextMessage abortMessage;
	
	private BullyEngine bullyEngine;

	public Communication() {
		Misc.writeLog("Looking up resources...");
		try {
			// Retrieveing administered objects
			Context ictx = new InitialContext();
			topic = (Topic)ictx.lookup("Bully");
			Misc.writeLog("Topic Bully found!");
			tcf = (TopicConnectionFactory)ictx.lookup("BullyConnectionFactory");
			Misc.writeLog("Topic connection factory found!");
			ictx.close();
			
			// Creating JMS objects
			Misc.writeLog("Creating JMS objects...");
			connection = tcf.createTopicConnection();
			
			// Pong JMS objects (used to catch a ping and reply with a pong)
			pongSession = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
			pongSubscriber = pongSession.createSubscriber(topic, Misc.TYPE + " = '" + Misc.PING + 
											"' AND " + Misc.DESTINATION + " = " + Bully.getClientID(), true);
			pongSubscriber.setMessageListener(new PingMessageListener(connection, topic));
			
			// JMS objects
			communicationSession = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
			electionPublisher = communicationSession.createPublisher(topic);
			electionReceiverSubscriber = communicationSession.createSubscriber(topic, Misc.TYPE + " = '" + Misc.ELECTION + "'", true);
			electionReceiverSubscriber.setMessageListener(new ElectionMessageListener());
			
			coordinatorSubscriber = communicationSession.createSubscriber(topic, Misc.TYPE + " = '" + Misc.COORDINATOR + "'", true);
			coordinatorSubscriber.setMessageListener(new CoordinatorMessageListener());
			
			abortSession = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
			abortSubscriber = abortSession.createSubscriber(topic, Misc.TYPE + " = '" + Misc.ABORT +
			 												"' AND " + Misc.DESTINATION + " = " + Bully.getClientID(), true);
			abortSubscriber.setMessageListener(new AbortMessageListener());
			
			abortMessage = communicationSession.createTextMessage();
			abortMessage.setStringProperty(Misc.TYPE, Misc.ABORT);
			abortMessage.setIntProperty(Misc.SOURCE, Bully.getClientID());
			
			// Creating engine thread
			bullyEngine = new BullyEngine(connection, topic);
			
		} catch (NamingException e) {
			Misc.error("An unrecoverable error happened Communication initialization.", e);
		} catch (JMSException e) {
			Misc.error("An unrecoverable error happened Communication initialization.", e);
		}
	}

	/**
	 * Starts the component.
	 */
	public void start() {
		Misc.writeLog("Starting connection");
		try {
			connection.start();
		} catch (JMSException e) {
			Misc.error("Error starting the connection when starting the Communication module." +
					" Printing stack trace and quitting.", e);
		}
		Misc.writeLog("Starting BullyEngine");
		new Thread(bullyEngine).start();
	}

	/**
	 * Stops the component.
	 */
	public void stop() {
		// Stop the pinger thread
		bullyEngine.exit();
		// Close JMS objects
		try {
			pongSubscriber.close();
			connection.close();
		} catch (JMSException e) {
			Misc.error("An unrecoverable error happened.", e);
		}
	}
	
	/**
	 * MessageListener implementation used to receive the election message
	 */
	class ElectionMessageListener implements MessageListener {

		@Override
		public void onMessage(Message message) {
			try {
				if (message.getIntProperty(Misc.SOURCE) < Bully.getClientID()) {
					// The election message has been sent by a client with
					// ID lower than this client. It is therefore to stop the
					// election replying to the message
					Misc.writeLog("ELECTION message received from Client " + message.getIntProperty(Misc.SOURCE) + " (lower ID)");
					abortMessage.setIntProperty(Misc.DESTINATION, message.getIntProperty(Misc.SOURCE));
					electionPublisher.publish(abortMessage);
					Misc.writeLog("ABORT message sent.");
					// Start a new election if not done previously
					synchronized (bullyEngine) {
						if (Bully.getInhibitElection() == false) {
							Misc.writeLog("Initiating an election");
							// Set the election inhibition to true
							Bully.setInhibitElection(true);
							// bullyEngine.startElection(electionPublisher);
							new Thread(bullyEngine.getElection()).start();
						}
					}
				} else {
					synchronized (bullyEngine) {
						Misc.writeLog("Election message received from Client " + message.getIntProperty(Misc.SOURCE) + " (higher ID)");
						// Set the election inhibition to true
						Bully.setInhibitElection(true);
					}
				}
			} catch (JMSException e) {
				Misc.error("An unrecoverable error happened.", e);
			}
		}
	}
	
	/**
	 * MessageListener implementation used to receive the new coordinator message
	 */
	class CoordinatorMessageListener implements MessageListener {
		
		@Override
		public void onMessage(Message message) {
			synchronized (bullyEngine) {
				int newCoordinator;
				try {
					newCoordinator = message.getIntProperty(Misc.COORDINATOR);
					// Set coordinator only if its coordinatorID is greater than the clientID,
					// otherwise start a new election
					if (newCoordinator > Bully.getClientID()) {
						// Set the new coordinator
						Bully.setCoordinator(newCoordinator);
						// Remove the inhibition. From now on this client
						// may start a new election
						Bully.setInhibitElection(false);
						Misc.writeLog("New coordinator is Client " + newCoordinator);
					} else {
						// We don't ever go in this section
						Misc.writeLog("Setting Coordinator to 0!!!!!");
						Bully.setCoordinator(0);
					}
				} catch (NumberFormatException e) {
					Misc.error("The COORDINATOR message does not contain a suitable integer", e);
				} catch (JMSException e) {
					Misc.error("The message received does not contain a COORDINATOR property", e);
				}
			}
		}
	}
	
	/**
	 * MessageListener implementation used to sense abort commands
	 */
	class AbortMessageListener implements MessageListener {
		@Override
		public void onMessage(Message message) {
			// bullyEngine.setAbort(true);
			bullyEngine.getElection().setAbort(true);
			Misc.writeLog("ABORT message received.");
		}
	}
	
	/**
	 * MessageListener implementation used to reply to pings
	 */
	class PingMessageListener implements MessageListener {
		
		private TopicSession session;
		private TopicPublisher publisher;
		
		public PingMessageListener(TopicConnection connection, Topic topic) {
			super();
			// Creating session and publisher to respond ping messages
			try {
				session = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
				publisher = session.createPublisher(topic);
			} catch (JMSException e) {
				Misc.error("An unrecoverable error happened.", e);
			}
		}

		@Override
		public void onMessage(Message message) {
			try {
				// Creating and sending reply 
				TextMessage pong = session.createTextMessage();
				pong.setStringProperty(Misc.TYPE, Misc.PONG);
				pong.setIntProperty(Misc.DESTINATION, message.getIntProperty(Misc.SOURCE));
				pong.setText(Misc.PONG);
				publisher.send(pong);
			} catch (JMSException e) {
				Misc.error("An unrecoverable error happened.", e);
			}
		}
	}
}
