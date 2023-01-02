package prf.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.User;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
	Optional<User> findByUsername(String username);
	
	@Query(value = "SELECT * FROM user WHERE email =:value OR username=:value",nativeQuery = true)
	User findByUsernameOrEmail(@Param("value") String value);
	
	Boolean existsByUsername(String username);
	Boolean existsByEmail(String email);

}