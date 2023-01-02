package prf.controllers;

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

	@GetMapping("/homePage")
	public String homePage() {
		return "homePage";
	}
}
