package prf.controllers;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import prf.entities.Notification;
import prf.payload.response.MessageResponse;
import prf.repositories.NotificationRepository;

@RestController
@RequestMapping("notifications")
public class Notificationcontroller {

	@Autowired
	NotificationRepository notifrepo;
	
	@GetMapping("find-all/{idUser}")
	@ResponseBody
	public List<Notification> findAllByUser(@PathVariable("idUser") Long idUser){
		return notifrepo.findAllNotificationsByUser(idUser);
	}
	
	@GetMapping("find-all-no-showed/{idUser}")
	@ResponseBody
	public List<Notification> findAllNoShowedByUser(@PathVariable("idUser") Long idUser){
		return notifrepo.findAllNoShowedNotificationsByUser(idUser);
	}
	
	@GetMapping("find-all-no-view/{idUser}")
	@ResponseBody
	public List<Notification> findAllNoViewByUser(@PathVariable("idUser") Long idUser){
		return notifrepo.findAllNoViewNotificationsByUser(idUser);
	}
	
	@SuppressWarnings("all")
	@PutMapping("edit-notification")
	@ResponseBody
	public Notification editNotification(@RequestBody Notification notif)
	{
		return notifrepo.save(notif);
	}
	
	@DeleteMapping("delete/{id}")
	@ResponseBody
	public ResponseEntity<Object> delete(@PathVariable("id") Long id){
		Optional<Notification> notificationOptional = notifrepo.findById(id);
		
		if(notificationOptional.isPresent()) {
			notifrepo.delete(notificationOptional.get());
			return ResponseEntity.ok().body(new MessageResponse("Suppression terminée!"));
		}
		
		return ResponseEntity.badRequest().body(new MessageResponse("Echec d'opération!"));
	}
}
