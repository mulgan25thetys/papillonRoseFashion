package prf.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.LikesPost;

@Repository
public interface LikesPostRepository extends JpaRepository<LikesPost, Long>{

	@Query(value ="SELECT *FROM likes_post WHERE (has_liked=1) AND (my_author_id=:userId AND my_post_id=:postId)",nativeQuery = true)
	List<LikesPost> getLikedPostByUser(@Param("userId") Long userId,@Param("postId") Long postId);
	
	@Query(value ="SELECT *FROM likes_post WHERE (has_liked=0) AND (my_author_id=:userId AND my_post_id=:postId)",nativeQuery = true)
	List<LikesPost> getUnLikedPostByUser(@Param("userId") Long userId,@Param("postId") Long postId);
	
	@Query(value ="SELECT *FROM likes_post WHERE my_author_id=:userId AND my_post_id=:postId LIMIT 1",nativeQuery = true)
	Optional<LikesPost> getLikesByPost(@Param("userId") Long userId,@Param("postId") Long postId);
	
	@Query(value = "SELECT COUNT(*) FROM likes_post WHERE my_post_id=:idPost AND has_liked=1",nativeQuery = true)
	Integer getNbrLikesByPost(@Param("idPost") Long idPost);
	
	@Query(value = "SELECT COUNT(*) FROM likes_post WHERE my_post_id=:idPost AND has_liked=0",nativeQuery = true)
	Integer getNbrUnLikesByPost(@Param("idPost") Long idPost);
}
