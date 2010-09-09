package distsys.bully.misc;

import distsys.bully.Bully;

public class Misc {

	// Message property identifiers
	public static final String TYPE = "TYPE";
	public static final String SOURCE = "SENDER";
	public static final String DESTINATION = "RECEIVER";
	
	// Message types
	public static final String PING = "PING";
	public static final String PONG = "PONG";
	public static final String ELECTION = "ELECTION";
	public static final String ABORT = "ABORT";
	public static final String COORDINATOR = "COORDINATOR";
	
	/**
	 * Prints a message on the standard output along with the clientID
	 * @param message message to print
	 */
	public static void writeLog(String message) {
		System.out.println(System.currentTimeMillis() + " - Client " + Bully.getClientID() + ": " + message);
	}
	
	public static void error(String message, Exception e) {
		System.err.println(System.currentTimeMillis() + " - Client " + Bully.getClientID() + ": " + message);
		System.err.println(System.currentTimeMillis() + " - Client " + Bully.getClientID() + ": Printing stack trace and quitting");
		e.printStackTrace();
		System.exit(-1);
	}
}
