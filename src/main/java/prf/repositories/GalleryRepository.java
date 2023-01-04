package prf.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.Gallery;

@Repository
public interface GalleryRepository extends JpaRepository<Gallery, Long>{

	@Query(value = "SELECT *FROM gallery WHERE post_id=:postId",nativeQuery = true)
	List<Gallery> getGalleriesForByPost(@Param("postId") Long id);
}
