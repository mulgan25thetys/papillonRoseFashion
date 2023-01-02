package prf.security.services;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import prf.entities.User;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class UserDetailsImpl implements UserDetails {
	
	private static final long serialVersionUID = 1L;
	private Long id;
	private String username;
	private String email;
	private String profile;
	private Boolean status;
	@JsonIgnore
	private String password;
	private Collection<? extends GrantedAuthority> authorities;
	
	public UserDetailsImpl(Long id,String username, String email, String password,String profile,Boolean status,
			Collection<? extends GrantedAuthority> authorities) {
		this.id = id;
		this.username = username;
		this.email = email;
		this.password = password;
		this.profile = profile;
		this.status = status;
		this.authorities = authorities;
	}
	public static UserDetailsImpl build(User user) {
		
		List<GrantedAuthority> authorities = new ArrayList<>();
		authorities.add(new SimpleGrantedAuthority(user.getRole().getName().name()));
			
		return new UserDetailsImpl(
				user.getId(), 
				user.getUsername(),
				user.getEmail(),
				user.getPassword(), 
				user.getProfile(),
				user.getStatus(),
				authorities);
	}
	
	
	public Long getId() {
		return id;
	}
	public String getEmail() {
		return email;
	}
	
	@Override
	public String getPassword() {
		return password;
	}
	@Override
	public String getUsername() {
		return username;
	}
	
	public String getProfile() {
		return profile;
	}
	public Boolean getStatus() {
		return status;
	}
	@Override
	public boolean isAccountNonExpired() {
		return true;
	}
	@Override
	public boolean isAccountNonLocked() {
		return true;
	}
	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}
	@Override
	public boolean isEnabled() {
		return true;
	}
	@Override
	public int hashCode() {
		return Objects.hash(id);
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		UserDetailsImpl other = (UserDetailsImpl) obj;
		return Objects.equals(id, other.id);
	}
	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		return authorities;
	}

	
}
