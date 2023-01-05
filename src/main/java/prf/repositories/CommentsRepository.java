package prf.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import prf.entities.Comments;

@Repository
public interface CommentsRepository extends JpaRepository<Comments, Long>{

}
