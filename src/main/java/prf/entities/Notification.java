package prf.entities;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Builder
@Getter
@Setter
@ToString
@AllArgsConstructor
public class Notification implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;
	private String object;
	@Column(length = 100000)
	private String message;
	private Boolean byMail;
	private Boolean inInternal;
	private Boolean isView;
	private Boolean isShowed;
	private Boolean sended;
	@Temporal(TemporalType.DATE)
	private Date dateNotification;
	
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getObject() {
		return object;
	}

	public void setObject(String object) {
		this.object = object;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Boolean getByMail() {
		return byMail;
	}

	public void setByMail(Boolean byMail) {
		this.byMail = byMail;
	}

	public Boolean getInInternal() {
		return inInternal;
	}

	public void setInInternal(Boolean inInternal) {
		this.inInternal = inInternal;
	}

	public Boolean getSended() {
		return sended;
	}

	public void setSended(Boolean sended) {
		this.sended = sended;
	}

	public Date getDateNotification() {
		return dateNotification;
	}

	public void setDateNotification(Date dateNotification) {
		this.dateNotification = dateNotification;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Notification() {
		super();
	}
	
	
}
