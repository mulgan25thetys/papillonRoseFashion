package prf.services;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import prf.entities.Post;

public interface IPostServices {

	Integer addPost(Post post,Long idCategory,Long idAuthor);
	
	Integer addPostImages(MultipartFile[] files,Long idPost);
	
	Post editPost(Post post);
	
	Post getPost(Long id);
	
	Integer deletePost(Long id);
	
	Page<Post> findAllPaging(Pageable pageable);
}
