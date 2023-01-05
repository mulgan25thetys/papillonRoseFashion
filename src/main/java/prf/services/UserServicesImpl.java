package prf.services;

import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.Random;

import javax.mail.MessagingException;
import javax.transaction.Transactional;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder;

import prf.controllers.FileController;
import prf.entities.User;
import prf.repositories.UserRepository;
import prf.security.services.UserDetailsImpl;

@Service
public class UserServicesImpl implements IUserServices{

	private final Path root = Paths.get("src/main/resources/uploads/profiles");
	
	private static final Logger log = Logger.getLogger(UserServicesImpl.class);
	
	Random random = new Random();
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private INotificationServices notificationServe;
	
	@Autowired
	PasswordEncoder encoder;
	
	@Override
	public User getAuthenticatedUSer() {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		
		if(auth.isAuthenticated()) {
			
			Optional<User> userOptional = userRepository.findById(((UserDetailsImpl)auth.getPrincipal()).getId());
			if(userOptional.isPresent()) {
				return userOptional.get();
			}
		}
		return null;
	}

	@Transactional
	public User addUser(User user) {
		user.setProfile("default-profile.jpg");
		user.setUsername(user.getEmail());
		user.setStatus(false);
		user.setDateCreation(new Date());
		String generatedPassword = this.generateRandomString();
		user.setPassword(encoder.encode(generatedPassword));
		userRepository.save(user);
		
		try {
			notificationServe.notifiyPersonnale(user,generatedPassword);
		} catch (MessagingException e) {
			e.printStackTrace();
		}
		return user;
	}
	
	private String generateRandomString() {
	    int leftLimit = 97; // letter 'a'
	    int rightLimit = 122; // letter 'z'
	    int targetStringLength = 10;

	    return random.ints(leftLimit, rightLimit + 1)
	      .limit(targetStringLength)
	      .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
	      .toString();
	}
	
	@Override
	public List<User> findAll() {
		return userRepository.findAll();
	}
	
	@Transactional
	public User editUser(User user) {
		
		
		user.setDateModification(new Date());
		
		return userRepository.save(user);
	}

	@Override
	public User editProfile(MultipartFile profile,Long idUser){
		User user = userRepository.findById(idUser).orElse(null);
		String filename = "";
		
		if(user !=null && profile !=null) {
			Optional<String> extension = Optional.ofNullable(profile.getOriginalFilename()).filter(f -> f.contains("."))
				      .map(f -> f.substring(profile.getOriginalFilename().lastIndexOf('.') + 1));
			if(extension.isPresent()) {
				filename = user.getUsername()+new Date().getTime()+"."+extension.get();
			}
			
			try {
			   Files.copy(profile.getInputStream(), this.root.resolve(filename));
			} catch (Exception e) {
				log.info("Could not store the file. Error: " + e.getMessage());
			    
			}
			user.setProfile(filename);
		}
		
		if(user != null) {
			user.setDateModification(new Date());
			return userRepository.save(user);
		}
		
		return user;
	}
	
	@Override
	public String getProfile(String filename) {
		String profileUrl = "";
		if(filename != null) {
			Resource profile = this.load(filename);
			try {
				if(profile != null) {
					profileUrl = MvcUriComponentsBuilder
					          .fromMethodName(FileController.class, "getFileForProfile", profile.getFilename()).build().toString();
				}
			} catch (Exception e) {
				log.debug(e);
			}
		}
		
		return profileUrl;
	}
	
	@Override
	public User getUser(Long id) {
		return userRepository.findById(id).orElse(null);
		
	}

	@Override
	public User changePassword(Long id, String newPassword) {
		User user = userRepository.findById(id).orElse(null);
		if(user !=null) {
			user.setPassword(new BCryptPasswordEncoder().encode(newPassword));
			user.setDateModification(new Date());
			return userRepository.save(user);
		}
		return null;
	}

	@Override
	public Resource load(String filename) {
	    try {
	      Path file = root.resolve(filename);
	      Resource resource = new UrlResource(file.toUri());
	      if (resource.exists() || resource.isReadable()) {
	        return resource;
	      } else {
	    	  log.info("could not read file");
	    	  return null;
	      }
	    } catch (MalformedURLException e) {
	      log.debug(e);
	      return null;
	    }	    
	  }
}
