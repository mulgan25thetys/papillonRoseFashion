package prf.services;

import java.util.List;

import org.springframework.core.io.Resource;
import org.springframework.web.multipart.MultipartFile;

import prf.entities.User;

public interface IUserServices {

	User getAuthenticatedUSer();
	
	User editUser(User user);
	
	User editProfile(MultipartFile profile,Long idUser);
	
	String getProfile(String filename);
	
	User getUser(Long id);
	
	User addUser(User user);
	
	Resource load(String filename);
	
	List<User> findAll();
	
    User changePassword(Long id, String newPassword);
	
}
