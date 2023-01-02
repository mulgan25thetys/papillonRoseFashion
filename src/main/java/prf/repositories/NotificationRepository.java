package prf.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.Notification;

@Repository
public interface NotificationRepository extends JpaRepository<Notification, Long>{

	@Query(value =  "SELECT * FROM notification n INNER JOIN user_notifications un "
			+ "ON n.id = un.notifications_id INNER JOIN user u ON u.id = un.user_id "
			+ "WHERE u.id =:idUser ORDER BY n.id DESC",nativeQuery = true)
	List<Notification> findAllNotificationsByUser(@Param("idUser") Long idUser);
	
	@Query(value =  "SELECT * FROM notification n INNER JOIN user_notifications un "
			+ "ON n.id = un.notifications_id INNER JOIN user u ON u.id = un.user_id "
			+ "WHERE u.id =:idUser AND (n.is_showed = 0 OR n.is_showed is null) ORDER BY n.id DESC ",nativeQuery = true)
	List<Notification> findAllNoShowedNotificationsByUser(@Param("idUser") Long idUser);
	
	@Query(value =  "SELECT * FROM notification n INNER JOIN user_notifications un "
			+ "ON n.id = un.notifications_id INNER JOIN user u ON u.id = un.user_id "
			+ "WHERE u.id =:idUser AND (n.is_view = 0 OR n.is_view is null) ORDER BY n.id DESC ",nativeQuery = true)
	List<Notification> findAllNoViewNotificationsByUser(@Param("idUser") Long idUser);
}
