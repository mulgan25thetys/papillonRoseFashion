package prf.repositories;

import java.util.List;
import java.util.Optional;

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
	
	@Query(value = "SELECT *FROM post WHERE is_published=1 ORDER BY id DESC",nativeQuery = true)
	List<Post> getPublishedPosts();
	
	@Query(value = "SELECT *FROM post WHERE is_published=1 ORDER BY id DESC LIMIT 3",nativeQuery = true)
	List<Post> getRecentsPosts();
	
	@Query(value = "SELECT *FROM post WHERE slug=:slug LIMIT 1",nativeQuery = true)
	Optional<Post> getPostBySlug(@Param("slug") String slug);
}

