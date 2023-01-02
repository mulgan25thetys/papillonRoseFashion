package prf.entities;

import java.io.Serializable;

import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

import org.hibernate.annotations.Proxy;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.FieldDefaults;

@Entity
@Proxy(lazy=false)
@Getter
@Setter
@FieldDefaults(level = AccessLevel.PRIVATE)
@ToString
@Builder
@AllArgsConstructor
public class User implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	private Long uniqueId;
	@NotBlank
	private String username;
	private String firstname;
	private String lastname;
	@NotBlank
	@Size(max = 50)
	@Email
	private String email;
	@Size(max = 200)
	private String password;
	private String profile;
	private Integer phone;
	private String country;
	private String city;
	private String address;
	@Temporal(TemporalType.DATE)
	private Date dateCreation;
	@Temporal(TemporalType.DATE)
	private Date dateModification;
	private Boolean status;
	private Integer code;
	@Temporal(TemporalType.DATE)
	private Date codeExpiryDate;
	private Boolean isBanned;
	@Temporal(TemporalType.DATE)
	private Date bannedAt;
	private String profession;
	private Boolean isOnline;
	private String lastIp;
	@Temporal(TemporalType.TIMESTAMP)
	private Date lastConnectionTime;
	
	@ManyToOne
	private Role role;
	
	@OneToMany(mappedBy = "author",fetch = FetchType.LAZY,cascade = CascadeType.ALL)
	private List<Post> posts;
	
	public User() {
	}
	public User(String username, String email, String password,String lastname,String firstname) {
		this.username = username;
		this.email = email;
		this.password = password;
		this.lastname = lastname;
		this.firstname = firstname;
	}
	
	public User(String username, String email, String password,String lastname,String firstname,String region,Integer contact) {
		this.username = username;
		this.email = email;
		this.password = password;
		this.lastname = lastname;
		this.firstname = firstname;
		this.city = region;
		this.phone = contact;
	}
}
