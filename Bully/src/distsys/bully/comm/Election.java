package distsys.bully.comm;

import javax.jms.JMSException;
import javax.jms.TextMessage;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;

import distsys.bully.Bully;
import distsys.bully.misc.Misc;

public class Election implements Runnable {

	private volatile boolean abort;
	
	private TopicPublisher publisher;
	
	private TextMessage electionMessage;
	private TextMessage coordinatorMessage;
	
	public Election(TopicSession session, javax.jms.Topic topic) {
		super();
		try {
			electionMessage = session.createTextMessage();
			electionMessage.setStringProperty(Misc.TYPE, Misc.ELECTION);
			electionMessage.setIntProperty(Misc.SOURCE, Bully.getClientID());
			
			coordinatorMessage = session.createTextMessage();
			coordinatorMessage.setStringProperty(Misc.TYPE, Misc.COORDINATOR);
			coordinatorMessage.setIntProperty(Misc.COORDINATOR, Bully.getClientID());
			
			publisher = session.createPublisher(topic);
		} catch (JMSException e) {
			Misc.error("An unexpected error occurred while starting an election.", e);
		}
	}
	
	public void setAbort(boolean abort) {
		this.abort = abort;
	}
	
	public boolean isAbort() {
		return abort;
	}
	
	@Override
	public void run() {
		try {
			// Send election request
			setAbort(false);
			publisher.publish(electionMessage);
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
		} catch (JMSException e) {
			Misc.error("An unrecoverable error happened.", e);
		}
	}
}
