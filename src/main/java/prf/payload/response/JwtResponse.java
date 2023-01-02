package prf.payload.response;

import java.util.ArrayList;
import java.util.List;

import groovy.transform.ToString;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@ToString
public class JwtResponse {

	private String token;
	private Long id;
	private String username;
	private String email;
	private String profile;
	private Boolean status;
	
	private List<String> roles = new ArrayList<>();

	public JwtResponse(String token, Long id,String username, String email, List<String> roles,String profile,Boolean status) {
		this.token = token;
		this.id = id;
		this.username = username;
		this.email = email;
		this.roles = roles;
		this.profile = profile;
		this.status = status;
	}
}
