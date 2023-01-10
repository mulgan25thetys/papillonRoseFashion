package prf.services;

import java.util.Date;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import prf.entities.Category;
import prf.repositories.CategoryRepository;

@Service
public class CategoryServicesImpl implements ICategoryServices{

	@Autowired CategoryRepository catRepo;
	
	@Override
	public Category addCategory(Category category) {
		
		if(category.getIsPremium() == null) {
			category.setIsPremium(false);
		}
		category.setSlug(category.getName().trim().replace(' ','-'));
		category.setAddedAt(new Date());
		return catRepo.save(category);
	}

	@Override
	public Category editCategory(Category category) {
		category.setSlug(category.getName().trim().replace(' ','-'));
		category.setUpdatedAt(new Date());
		return catRepo.save(category);
	}

	@Override
	public Category getCategory(Long id) {
		Optional<Category> categoryOptional = catRepo.findById(id);
		
		if(categoryOptional.isPresent()) {
			return categoryOptional.get();
		}
		return categoryOptional.orElse(null);
	}

	@Transactional
	public Integer deleteCategory(Long id) {
		Optional<Category> categoryOptional = catRepo.findById(id);
		
		if(categoryOptional.isPresent()) {
			catRepo.delete(categoryOptional.get());
			return 0;
		}
		return -1;
	}

	@Override
	public Page<Category> findAllPaging(Pageable pageable) {
		return catRepo.findAll(pageable);
	}

}
