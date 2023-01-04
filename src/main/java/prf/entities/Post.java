package prf.entities;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotBlank;

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
public class Post implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	@NotBlank
	private String title;
	@Column(length = 100000)
	@NotBlank
	private String content;
	@Column(length = 1000)
	private String slug;
	private Boolean isPublished;
	private Boolean isDownloaded;
	private Boolean isShared;
	private Integer views;
	private Integer likes;
	private Integer unlikes;
	private Integer numberShares;
	private Boolean isPremium;
	@Temporal(TemporalType.TIMESTAMP)
	private Date addedAt;
	@Temporal(TemporalType.TIMESTAMP)
	private Date updatedAt;

	@ManyToOne
	private User author;
	
	@ManyToOne
	private Category category;
	
	@OneToMany(mappedBy = "post",fetch = FetchType.LAZY,cascade = CascadeType.ALL)
	private List<Gallery> galleries;

	public Post() {
		super();
	}
	
	
}