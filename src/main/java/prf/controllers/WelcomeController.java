package prf.controllers;

import javax.servlet.http.HttpServletRequest;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/")
public class WelcomeController {

	@GetMapping("/")
	public String welcomePage() {
		return "Welcome to papillon Rose Fashion api";
	}

	@GetMapping("/home-page")
	public String homePage() {
		return "homePage";
	}
	
	@GetMapping("/current-user")
	 public Object currentUserName(HttpServletRequest request) {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		return auth.getPrincipal();
	}
}
