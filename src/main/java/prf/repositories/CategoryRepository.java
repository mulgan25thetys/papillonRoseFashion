package prf.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import prf.entities.Category;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long>{

	Boolean existsByName(String name);
	
	Optional<Category> findByName(String name);
	
	@Query(value = "SELECT * FROM category WHERE name=:name",nativeQuery = true)
	Category getCategoryByName(@Param("name") String name);
	
}
