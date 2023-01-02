package prf.services;

import javax.mail.MessagingException;

import prf.entities.User;

public interface INotificationServices {

	public void sendMailWithCode(User user,Boolean withCode) throws MessagingException;
	
	public void notifiyPersonnale(User user,String password) throws MessagingException;
}
