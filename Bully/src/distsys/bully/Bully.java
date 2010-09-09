package distsys.bully;

import java.io.IOException;

import distsys.bully.comm.Communication;
import distsys.bully.misc.Misc;

public class Bully {
	
	private static int clientID;
	private static int coordinator = 1;
	private static boolean inhibitElection = false;

	public static void main(String[] args) {
		
		if (args.length < 1) {
			System.out.println("Usage: bully <clientID>");
			System.exit(0);
		}
		
		clientID = Integer.parseInt(args[0]);
		
		Misc.writeLog("Client " + clientID);
		
		Communication comm = new Communication();
		comm.start();
		
		Misc.writeLog("Press enter to stop...");
		try {
			System.in.read();
		} catch (IOException e) {
			e.printStackTrace();
		}
		comm.stop();
		Misc.writeLog("Bully client terminated");
	}
	
	public static synchronized int getClientID() {
		return clientID;
	}
	
	public static synchronized void setCoordinator(int newCoordinator) {
		coordinator = newCoordinator;
	}
	
	public static int getCoordinator() {
		return coordinator;
	}
	
	public static boolean getInhibitElection() {
		return inhibitElection;
	}
	
	public static void setInhibitElection(boolean newInhibitElection) {
		inhibitElection = newInhibitElection;
	}
}
