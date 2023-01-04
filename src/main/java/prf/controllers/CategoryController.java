package prf.controllers;

import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import prf.repositories.CategoryRepository;
import prf.services.ICategoryServices;

import prf.entities.Category;
import prf.payload.response.MessageResponse;

@RestController
@RequestMapping("categories")
public class CategoryController {

	private static final Logger log = Logger.getLogger(CategoryController.class);
	
	private String success4GenericMessage = "L'opération a été bien éffectuée!";
	private String error4GenericMessage = "Une Erreur s'est produite veuillez ressayer!";
	private String error4ExistingCategoryMessage = "Cette categorie de publication exist déja!";
	private String error4NonExistingCategoryMessage = "Cette categorie de publication n'exist pas!";
	
	@Autowired
	CategoryRepository cateRepo;
	
	@Autowired
	ICategoryServices cateServe;
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@GetMapping("/find-all-paging")
	@ResponseBody
	public Page<Category> findAllPaging(@RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size){
		Pageable paging = PageRequest.of(page, size);
		return cateServe.findAllPaging(paging);
	}
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@GetMapping("/find-all")
	@ResponseBody
	public List<Category> findAll(){
		return cateRepo.findAll();
	}
	
	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@PostMapping("/add-category")
	@ResponseBody
	public ResponseEntity<Object> addCategory(Exception ex, 
            HttpServletRequest request, HttpServletResponse response,@RequestBody Category category)
	{
		Boolean success=false;
		
		try {
			if(Boolean.TRUE.equals(cateRepo.existsByName(category.getName()))) {
				return ResponseEntity.badRequest().body(new MessageResponse(error4ExistingCategoryMessage));
			}
			
			cateServe.addCategory(category);
			success=true;
		} catch (Exception e) {
			log.debug(e);
		}
		
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4GenericMessage));
		}else {
			return ResponseEntity.ok().body(new MessageResponse("Une categorie de publication a été ajoutée!"));
		}	
	}
	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@PutMapping("/edit-category")
	@ResponseBody
	public ResponseEntity<Object> editCategory(@RequestBody Category category)
	{
		Boolean success=false;
		if(category.getId() == null) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4GenericMessage));
		}
		
		if(Boolean.TRUE.equals(cateRepo.existsByName(category.getName()))) {
			Optional<Category> categoryOption = cateRepo.findByName(category.getName());
			if(categoryOption.isPresent() && !categoryOption.get().getId().equals(category.getId())) {
				return ResponseEntity.badRequest().body(new MessageResponse(error4ExistingCategoryMessage+" avec ce nom!"));
			}
		}
		
		try {
			cateServe.editCategory(category);
			success=true;
		} catch (Exception e) {
			log.debug(e);
		}
		
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4GenericMessage));
		}else {
			return ResponseEntity.ok().body(new MessageResponse("Une categorie de publication a été modifiée!"));
		}	
	}
	

	@GetMapping("/get-category/{id}")
	@ResponseBody
	public ResponseEntity<Object> getCategory(@PathVariable("id") Long id){
		
		if(Boolean.FALSE.equals(cateRepo.existsById(id))) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4NonExistingCategoryMessage));
		}
		
		return ResponseEntity.ok().body(cateServe.getCategory(id));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@DeleteMapping("/delete-category/{id}")
	@ResponseBody
	public ResponseEntity<Object> deleteCategory(@PathVariable("id") Long id)
	{
		
		if(Boolean.FALSE.equals(cateRepo.existsById(id))) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4NonExistingCategoryMessage));
		}
		
		int result = cateServe.deleteCategory(id);
		
		if(result == -1) {
			return ResponseEntity.badRequest().body(new MessageResponse(error4GenericMessage));
		}
		
		return ResponseEntity.ok().body(new MessageResponse(success4GenericMessage));
	}
}
