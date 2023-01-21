package prf.services;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import prf.entities.Comments;
import prf.entities.Post;
import prf.entities.User;

public interface IPostServices {

	Integer addPost(Post post,Long idCategory,User author);
	
	Integer addPostImages(MultipartFile[] files,Long idPost);
	
	Post editPost(Post post);
	
	Post getPost(Long id);
	
	Integer deletePost(Long id);
	
	Page<Post> findAllPaging(Pageable pageable);
	
	void likesPost(Long idPost,int value,User author);
	
	Post viewsPost(String slug,User author);
	
	Comments commentPost(Long idPost,User author,Comments comment);
}
 