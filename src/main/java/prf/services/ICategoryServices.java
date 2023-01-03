package prf.services;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import prf.entities.Category;

public interface ICategoryServices {

	Category addCategory(Category category);
	
	Category editCategory(Category category);
	
	Category getCategory(Long id);
	
	Integer deleteCategory(Long id);
	
	Page<Category> findAllPaging(Pageable pageable);
}
