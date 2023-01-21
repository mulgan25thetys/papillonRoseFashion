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
import prf.entities.Comments;
import prf.entities.ERole;
import prf.entities.Gallery;
import prf.entities.LikesPost;
import prf.entities.Post;
import prf.entities.User;
import prf.entities.ViewsPost;
import prf.repositories.CategoryRepository;
import prf.repositories.CommentsRepository;
import prf.repositories.GalleryRepository;
import prf.repositories.LikesPostRepository;
import prf.repositories.PostRepository;
import prf.repositories.UserRepository;
import prf.repositories.ViewsPostRepository;

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
	
	@Autowired
	LikesPostRepository likesRepo;
	
	@Autowired
	ViewsPostRepository viewRepo;
	
	@Autowired
	CommentsRepository commentRepo;
	
	//Add post 
	@Transactional
	public Integer addPost(Post post,Long idCategory,User author) {
		Optional<Category> cateOptional = cateRepo.findById(idCategory);
		
		if(cateOptional.isPresent() && !author.getRole().getName().equals(ERole.ROLE_CLIENT)) {
			Category category = cateOptional.get();
			
			if(post.getIsPremium() == null) {
				post.setIsPremium(false);
			}
			
			if(post.getIsDownloaded() == null) {
				post.setIsDownloaded(false);
			}
			
			if(post.getIsShared() == null) {
				post.setIsShared(false);
			}
			
			if(post.getIsPublished() == null) {
				post.setIsPublished(false);
			}
			post.setTitle(post.getTitle().trim());
			post.setAuthor(author);
			post.setCategory(category);
			post.setSlug(post.getTitle().trim().replace(' ', '-') );
			post.setNumberShares(0);
			post.setAddedAt(new Date());
			
			Post addedPost = postRepo.save(post);

			Gallery gallery = new Gallery();
			gallery.setIsDefault(true);
			gallery.setHasShowed(true);
			gallery.setName("post-default.jpg");
			gallery.setAddedAt(new Date());
			gallery.setPost(addedPost);
			
			gallRepo.save(gallery);
			return 0;
		}
		return -1;
	}

	//add Images Galleries to a post
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
	
	//Edit post
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

	//get Post by Id
	@Override
	public Post getPost(Long id) {
		Optional<Post> postOptional = postRepo.findById(id);
		
		if(postOptional.isPresent()) {
			Post post = postOptional.get();
					
			return this.preparePostDetails(post);
		}
		return postOptional.orElse(null);
	}
	
	private Post preparePostDetails(Post post) {
		for (Gallery gallery : post.getGalleries()) {
			String url = MvcUriComponentsBuilder
			          .fromMethodName(FileController.class, "getFileForPosts", gallery.getName()).build().toString();
			gallery.setUrl(url);
			
			if(Boolean.TRUE.equals(gallery.getIsDefault())) {
				post.setProfileDefault(gallery.getUrl());
			}
		}
		String url = MvcUriComponentsBuilder
		          .fromMethodName(FileController.class, "getFileForProfile", post.getAuthor().getProfile()).build().toString();
		post.getAuthor().setProfileUrl(url);
		
		for(Comments comment : post.getComments()) {
			String urlc = MvcUriComponentsBuilder
			          .fromMethodName(FileController.class, "getFileForProfile", comment.getAuthor().getProfile()).build().toString();
			comment.getAuthor().setProfileUrl(urlc);
		}
		post.setViews(viewRepo.getNbrViewsByPost(post.getId()));
		post.setUnlikes(likesRepo.getNbrUnLikesByPost(post.getId()));
		post.setLikes(likesRepo.getNbrLikesByPost(post.getId()));
		
		return post;
	}
	
	//Delete Post

	@Transactional
	public Integer deletePost(Long id) {
		Optional<Post> postOptional = postRepo.findById(id);
		Boolean success =false;
		if(postOptional.isPresent()) {
			success = this.deleteImagesByPost(postOptional.get());
			
			if(Boolean.TRUE.equals(success)) {
				postRepo.delete(postOptional.get());
				return 0;
			}
		}
		return -1;
	}
	
	private Boolean deleteImagesByPost(Post post) {
		Boolean success = false;
		if(post.getGalleries().isEmpty()) {
			success = true;
		}else {
			for (Gallery gallery : post.getGalleries()) {
				Path file =  Paths.get(ROOT_4_PATH).resolve(gallery.getName());
				try {
					if(Boolean.TRUE.equals(Files.deleteIfExists(file))) {
						success = true;
					}
				} catch (IOException e) {
					log.debug(e);
				}
			}
		}
		return success;
	}

	//Find All posts with pagination
	@Override
	public Page<Post> findAllPaging(Pageable pageable) {
		return postRepo.findAll(pageable);
	}

	@Override
	public void likesPost(Long idPost, int value,User author) {
		Post post = this.getPost(idPost);
		LikesPost like = new LikesPost();
		List<LikesPost> likesPosts = likesRepo.getLikedPostByUser(author.getId(),idPost);
		List<LikesPost> unLikesPosts = likesRepo.getUnLikedPostByUser(author.getId(),idPost);
		
		Boolean success=false;
		switch (value) {
		case 1:
			
			if(Boolean.FALSE.equals(unLikesPosts.isEmpty())) {
				this.updateLikesPost(author.getId(), idPost, true);

			}
			else if(Boolean.TRUE.equals(likesPosts.isEmpty()) && post!=null) {
				like.setHasLiked(true);
				success=true;
			}
			break;
		case 0:
			if(Boolean.FALSE.equals(likesPosts.isEmpty())) {
				this.updateLikesPost(author.getId(), idPost, false);
			}
			else if(Boolean.TRUE.equals(unLikesPosts.isEmpty()) && post!=null) {
				like.setHasLiked(false);
				success=true;
			}
			break;
		default:
			break;
		}
		
		if(Boolean.TRUE.equals(success)) {
			like.setLikesAt(new Date());
			like.setMyPost(post);
			like.setMyAuthor(author);
			likesRepo.save(like);
		}
	}
	
	private void updateLikesPost(Long idAuthor,Long idPost,Boolean value) {
		Optional<LikesPost> likeOptional = likesRepo.getLikesByPost(idAuthor, idPost);
		if(likeOptional.isPresent()) {
			LikesPost like = likeOptional.get();
			like.setHasLiked(value);
			like.setLikesAt(new Date());
			likesRepo.save(like);
		}
	}
	
	private Post getPostBySlug(String slug) {
		Optional<Post> postOptional = postRepo.getPostBySlug(slug);
		
		if(postOptional.isPresent()) {
			return postOptional.get();
		}
		return postOptional.orElse(null);
	}
	
	//View Post and update database

	@Override
	public Post viewsPost(String slug, User author) {
		
		Post post = this.getPostBySlug(slug);
		if(post != null) {
			Optional<ViewsPost> viewsOptional = viewRepo.getViewsForPostByUser(author.getId(), post.getId());
			
			ViewsPost view = new ViewsPost();
			
			if(Boolean.FALSE.equals(viewsOptional.isPresent())) {
				
				view.setMyAuthor(author);
				view.setMyPost(post);
			}
			else {
				view = viewsOptional.get();
			}
		
			view.setViewAt(new Date());
			viewRepo.save(view);
			
			post.setViews(viewRepo.getNbrViewsByPost(post.getId()));
			return this.preparePostDetails(post);
		}
		return null;
	}

	@Override
	public Comments commentPost(Long idPost, User author, Comments comment) {
		Post post = this.getPost(idPost);
		
		comment.setAuthor(author);
		comment.setPost(post);
		comment.setCommentAt(new Date());
		return commentRepo.save(comment);
	}
}
