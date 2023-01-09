package prf.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.Post;

@Repository
public interface PostRepository extends JpaRepository<Post, Long> {

	Boolean existsByTitle(String title);
	
	@Query(value = "SELECT *FROM post WHERE category_id=:idCate",nativeQuery = true)
	List<Post> getPostsByCategory(@Param("idCate") Long idCate);
}
