package prf.controllers;

import java.util.List;

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
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder;

import prf.entities.Gallery;
import prf.entities.Post;
import prf.payload.response.MessageResponse;
import prf.repositories.CategoryRepository;
import prf.repositories.GalleryRepository;
import prf.repositories.PostRepository;
import prf.repositories.UserRepository;
import prf.services.IPostServices;

@RestController
@RequestMapping("posts")
public class PostController {
	
	private static final Logger log = Logger.getLogger(PostController.class);
	
	private String success4Message = "L'operation a été bien realisée!";
	private String genericMessage4Error = "Une erreur s'est produite veuillez ressayer!";
	private String genericMessageError4ExistingPost = "Cette publication exist déja!";
	private String genericMessageError4NonExistingPost = "Cette publication n'exist pas!";
	private String genericMessageError4Missing = "Cette requeste est incomplete";
			
	@Autowired
	CategoryRepository cateRepo;
	
	@Autowired
	PostRepository postRepo;
	
	@Autowired
	GalleryRepository gallRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@Autowired
	IPostServices postServe;
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN','ROLE_CLIENT')")
	@GetMapping("/find-all-paging")
	@ResponseBody
	public Page<Post> findAllPaging(@RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size){
		Pageable paging = PageRequest.of(page, size);
		return postServe.findAllPaging(paging);
	}

	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN','ROLE_CLIENT')")
	@GetMapping("/find-all")
	@ResponseBody
	public List<Post> findAll(){
		List<Post> posts = postRepo.findAll();
		try {
			for (Post post : posts) {
				for (Gallery gallery : post.getGalleries()) {
					String url = MvcUriComponentsBuilder
					          .fromMethodName(FileController.class, "getFileForPosts", gallery.getName()).build().toString();
					gallery.setUrl(url);
				}
				
				String url = MvcUriComponentsBuilder
				          .fromMethodName(FileController.class, "getFileForProfile", post.getAuthor().getProfile()).build().toString();
				post.getAuthor().setProfile(url);
			}
		} catch (Exception e) {
			log.debug(e);
		}
		return posts;
	}
	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@PostMapping("add-post/{idCategory}/{idAuthor}")
	@ResponseBody
	public ResponseEntity<Object> addPost(@RequestBody Post post,@PathVariable("idCategory") Long idCategory,
			@PathVariable("idAuthor") Long idAuthor){
		Boolean success = false;
		try {
			if(Boolean.TRUE.equals(postRepo.existsByTitle(post.getTitle()))) {
				return ResponseEntity.badRequest().body(new MessageResponse(genericMessageError4ExistingPost));
			}
			
			if(postServe.addPost(post, idCategory, idAuthor) == 0) {
				success=true;
			}
			
		} catch (Exception e) {
			log.debug(e);
		}
		
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(genericMessage4Error));
		}else {
			return ResponseEntity.ok().body(new MessageResponse(success4Message));
		}
	}
	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@PutMapping("edit-post")
	@ResponseBody
	public ResponseEntity<Object> editPost(@RequestBody Post post){
		Boolean success = false;
		try {
			if(post.getId() ==null) {
				return ResponseEntity.badRequest().body(new MessageResponse(genericMessageError4Missing));
			}
			
			postServe.editPost(post);
			success=true;
			
		} catch (Exception e) {
			log.debug(e);
		}
		
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(genericMessage4Error));
		}else {
			return ResponseEntity.ok().body(new MessageResponse(success4Message));
		}
	}
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@PostMapping("add-post-images/{idPost}")
	@ResponseBody
	public ResponseEntity<Object> addPostImages(@RequestParam("files") MultipartFile[] files,
			@PathVariable("idPost") Long idPost){
		Boolean success = false;
		
		try {
			if(Boolean.FALSE.equals(postRepo.existsById(idPost))) {
				return ResponseEntity.badRequest().body(new MessageResponse(genericMessageError4NonExistingPost));
			}
			
			if(postServe.addPostImages(files, idPost) == 0) {
				success=true;
			}
		} catch (Exception e) {
			log.debug(e);
		}
		
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(genericMessage4Error));
		}else {
			return ResponseEntity.ok().body(new MessageResponse(success4Message));
		}
	}
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN','ROLE_CLIENT')")
	@GetMapping("get-post/{id}")
	@ResponseBody
	public ResponseEntity<Object> getPost(@PathVariable("id") Long id){
		
		try {
			if(Boolean.FALSE.equals(postRepo.existsById(id))) {
				return ResponseEntity.badRequest().body(new MessageResponse(genericMessageError4NonExistingPost));
			}
		} catch (Exception e) {
			log.debug(e);
		}
		return ResponseEntity.ok().body(postServe.getPost(id));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_AGENT','ROLE_ADMIN')")
	@DeleteMapping("delete-post/{id}")
	@ResponseBody
	public ResponseEntity<Object> deletePost(@PathVariable("id") Long id){
		Boolean success=false;
		try {
			if(Boolean.FALSE.equals(postRepo.existsById(id))) {
				return ResponseEntity.badRequest().body(new MessageResponse(genericMessageError4NonExistingPost));
			}
			if(postServe.deletePost(id) == 0) {
				success = true;
			}
		} catch (Exception e) {
			log.debug(e);
		}
		if(Boolean.FALSE.equals(success)) {
			return ResponseEntity.badRequest().body(new MessageResponse(genericMessage4Error));
		}else {
			return ResponseEntity.ok().body(new MessageResponse(success4Message));
		}
	}
}
