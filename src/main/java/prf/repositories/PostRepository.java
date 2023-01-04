package prf.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import prf.entities.Post;

@Repository
public interface PostRepository extends JpaRepository<Post, Long> {

	Boolean existsByTitle(String title);
}
