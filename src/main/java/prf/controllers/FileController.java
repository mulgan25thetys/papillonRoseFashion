package prf.controllers;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder;

import prf.entities.Attachements;
import prf.payload.response.MessageResponse;
import prf.services.IFileStorageServices;

@RestController
@RequestMapping("/joints")
public class FileController {

	@Autowired
	IFileStorageServices storageService;
	
	@PreAuthorize("hasAnyRole('ROLE_CLIENT','ROLE_AGENT','ROLE_ADMIN')")
	  @PostMapping("/upload")
	  public ResponseEntity<MessageResponse> uploadFile(@RequestParam("file") MultipartFile file) {
	    String message = "";
	    try {
	      storageService.save(file);
	      message = "Uploaded the file successfully: " + file.getOriginalFilename();
	      return ResponseEntity.status(HttpStatus.OK).body(new MessageResponse(message));
	    } catch (Exception e) {
	      message = "Could not upload the file: " + file.getOriginalFilename() + "!";
	      return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).body(new MessageResponse(message));
	    }
	  }
	  
	@PreAuthorize("hasAnyRole('ROLE_CLIENT','ROLE_AGENT','ROLE_ADMIN')")
	  @GetMapping("/files")
	  public ResponseEntity<List<Attachements>> getListFiles() {
	    List<Attachements> fileInfos = storageService.loadAll().map(path -> {
	      String filename = path.getFileName().toString();
	      String url = MvcUriComponentsBuilder
	          .fromMethodName(FileController.class, "getFile", path.getFileName().toString()).build().toString();
	      return new Attachements(filename, url);
	    }).collect(Collectors.toList());
	    return ResponseEntity.status(HttpStatus.OK).body(fileInfos);
	  }

	@PreAuthorize("hasAnyRole('ROLE_CLIENT','ROLE_AGENT','ROLE_ADMIN')")
	  @GetMapping("/files/{filename:.+}")
	  @ResponseBody
	  public ResponseEntity<Resource> getFile(@PathVariable String filename) {
	    Resource file = storageService.load(filename);
	    return ResponseEntity.ok()
	        .header(HttpHeaders.CONTENT_DISPOSITION, "attachments; filename=\"" + file.getFilename() + "\"").body(file);
	  }
	  
	  @PreAuthorize("hasAnyRole('ROLE_CLIENT','ROLE_AGENT','ROLE_ADMIN')")
	  @GetMapping("/profiles/{filename:.+}")
		@ResponseBody
		public ResponseEntity<Resource> getFileForProfile(@PathVariable String filename) throws IOException {
		    Resource file = storageService.loadProfiles(filename);
		    Path path = file.getFile()
	              .toPath();
		    
		    return ResponseEntity.ok()
	              .header(HttpHeaders.CONTENT_TYPE, Files.probeContentType(path))
	              .header(HttpHeaders.CONTENT_DISPOSITION, "attachments profiles; filename=\"" + file.getFilename() + "\"")
	              .body(file);
		  }
	  
	  @PreAuthorize("hasAnyRole('ROLE_CLIENT','ROLE_AGENT','ROLE_ADMIN')")
	  @GetMapping("/posts/{filename:.+}")
		@ResponseBody
		public ResponseEntity<Resource> getFileForPosts(@PathVariable String filename) throws IOException {
		    Resource file = storageService.loadPostFiles(filename);
		    Path path = file.getFile()
	              .toPath();
		    
		    return ResponseEntity.ok()
	              .header(HttpHeaders.CONTENT_TYPE, Files.probeContentType(path))
	              .header(HttpHeaders.CONTENT_DISPOSITION, "attachments posts; filename=\"" + file.getFilename() + "\"")
	              .body(file);
		  }
}
