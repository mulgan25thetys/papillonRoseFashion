package prf.services;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder;

import prf.controllers.FileController;
import prf.entities.Category;
import prf.entities.ERole;
import prf.entities.Gallery;
import prf.entities.Post;
import prf.entities.User;
import prf.repositories.CategoryRepository;
import prf.repositories.GalleryRepository;
import prf.repositories.PostRepository;
import prf.repositories.UserRepository;

@Service
public class PostServicesImpl implements IPostServices {

	private static final Logger log = Logger.getLogger(PostServicesImpl.class);
	private static final String ROOT_4_PATH = "src/main/resources/uploads/posts/";
	
	@Autowired
	CategoryRepository cateRepo;
	
	@Autowired
	PostRepository postRepo;
	
	@Autowired
	GalleryRepository gallRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@Transactional
	public Integer addPost(Post post,Long idCategory, Long idAuthor) {
		Optional<Category> cateOptional = cateRepo.findById(idCategory);
		Optional<User> userOptional = userRepo.findById(idAuthor);
		
		if(cateOptional.isPresent() && userOptional.isPresent() && !userOptional.get().getRole().getName().equals(ERole.ROLE_CLIENT)) {
			User author = userOptional.get();
			Category category = cateOptional.get();
			
			if(post.getIsPremium() == null) {
				post.setIsPremium(false);
			}
			
			if(post.getIsPublished() == null) {
				post.setIsPublished(false);
			}
			post.setTitle(post.getTitle().trim());
			post.setAuthor(author);
			post.setCategory(category);
			post.setSlug(post.getTitle().trim().replace(' ', '-') );
			post.setAddedAt(new Date());
			
			postRepo.save(post);

			return 0;
		}
		return -1;
	}

	@Override
	public Integer addPostImages(MultipartFile[] files, Long idPost) {
		Optional<Post> postOptional = postRepo.findById(idPost);
		
		if(postOptional.isPresent()) {
           List<String> fileNames = new ArrayList<>();
			
			try 
			{
            // read and write the file to the local folder
            Arrays.asList(files).stream().forEach(file -> {
                byte[] bytes = new byte[0];
                try {
                    bytes = file.getBytes();
                    String fileNewName = new Date().getTime()+"-"+"post"+postOptional.get().getId()+"-"+file.getOriginalFilename();
                    
                    Files.write(Paths.get(ROOT_4_PATH + fileNewName), bytes);
                    fileNames.add(fileNewName);
                } catch (IOException e) {
                	log.debug(e);
                }
            });

			} catch (Exception e) {
				log.debug("Exception to upload files!");
			}
			
			Post post = postOptional.get();
			for (int i = 0; i < fileNames.size(); i++) {
				String galleryName = fileNames.get(i);
				
				Gallery gallery = new Gallery();
				gallery.setIsDefault(false);
				
				if (i==0 && post.getGalleries().isEmpty()) {
					gallery.setIsDefault(true);
				}
				
				gallery.setHasShowed(true);
				gallery.setName(galleryName);
				gallery.setAddedAt(new Date());
				gallery.setPost(post);
				
				gallRepo.save(gallery);
			}
			return 0;
		}
		return -1;
	}
	
	@Transactional
	public Post editPost(Post post) {
		
		post.setTitle(post.getTitle().trim());
		post.setSlug(post.getTitle().replace(' ', '-'));
		post.setUpdatedAt(new Date());
		
		List<Gallery> galleries = gallRepo.getGalleriesForByPost(post.getId());
		Post updatedPost = postRepo.save(post);
		
		if(!galleries.isEmpty()) {
			for (Gallery gallery : galleries) {
				gallery.setPost(updatedPost);
				gallRepo.save(gallery);
			}
		}
		
		return updatedPost;
	}

	@Override
	public Post getPost(Long id) {
		Optional<Post> postOptional = postRepo.findById(id);
		
		if(postOptional.isPresent()) {
			Post post = postOptional.get();
					for (Gallery gallery : post.getGalleries()) {
						String url = MvcUriComponentsBuilder
						          .fromMethodName(FileController.class, "getFileForPosts", gallery.getName()).build().toString();
						gallery.setUrl(url);
					}
			return post;
		}
		return postOptional.orElse(null);
	}

	@Transactional
	public Integer deletePost(Long id) {
		Optional<Post> postOptional = postRepo.findById(id);
		Boolean success =false;
		if(postOptional.isPresent()) {
		
			for (Gallery gallery : postOptional.get().getGalleries()) {
				Path file =  Paths.get(ROOT_4_PATH).resolve(gallery.getName());
				try {
					if(Boolean.TRUE.equals(Files.deleteIfExists(file))) {
						success = true;
					}
				} catch (IOException e) {
					log.debug(e);
				}
			}
			if(Boolean.TRUE.equals(success)) {
				postRepo.delete(postOptional.get());
				return 0;
			}
		}
		return -1;
	}

	@Override
	public Page<Post> findAllPaging(Pageable pageable) {
		return postRepo.findAll(pageable);
	}
}
