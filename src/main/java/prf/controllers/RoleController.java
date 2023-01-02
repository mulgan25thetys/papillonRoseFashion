package prf.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import prf.entities.Role;
import prf.payload.response.MessageResponse;
import prf.repositories.RoleRepository;

@RestController
@RequestMapping("roles")
public class RoleController {

	@Autowired
	RoleRepository roleRepo;
	
	@PreAuthorize("hasAnyRole('ROLE_ADMIN')")
	@GetMapping("find-all")
	@ResponseBody
	public List<Role> findAll(){
		return roleRepo.findAll();
	}
	
	@PreAuthorize("hasAnyRole('ROLE_ADMIN')")
	@GetMapping("/find/{id}")
	@ResponseBody
	public ResponseEntity<Object> find(@PathVariable("id") Integer id) {
		
		if(Boolean.FALSE.equals(roleRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("Le role recherché n'existe pas!"));
		}
		return ResponseEntity.ok().body(roleRepo.findById(id));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_ADMIN')")
	@SuppressWarnings("all")
	@PostMapping("add")
	@ResponseBody
	public ResponseEntity<Object> add(@RequestBody Role role){
		if(Boolean.TRUE.equals(roleRepo.existsByName(role.getName().name()))) {
			return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).body(new MessageResponse("Le role existe deja!"));
		}
		
		return ResponseEntity.ok().body(roleRepo.save(role));
	}
}
