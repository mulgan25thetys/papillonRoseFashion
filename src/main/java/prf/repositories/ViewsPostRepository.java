package prf.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.ViewsPost;

@Repository
public interface ViewsPostRepository extends JpaRepository<ViewsPost, Long> {

	@Query(value = "SELECT *FROM views_post WHERE my_author_id=:idUser AND my_post_id=:idPost LIMIT 1",nativeQuery = true)
	Optional<ViewsPost> getViewsForPostByUser(@Param("idUser") Long idAuthor,@Param("idPost") Long idPost);
	
	@Query(value = "SELECT COUNT(*) FROM views_post WHERE my_post_id=:idPost",nativeQuery = true)
	Integer getNbrViewsByPost(@Param("idPost") Long idPost);
}
