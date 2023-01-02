package prf.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import prf.entities.ERole;
import prf.entities.Role;

@Repository
public interface RoleRepository extends JpaRepository<Role, Integer>{

	Optional<Role> findByName(ERole name);
	
	@Query(value = "SELECT *FROM Role WHERE name =:rolename ",nativeQuery = true)
	public Role getRoleByName(@Param("rolename") String name);
	
	Boolean existsByName(String name);
	
}
