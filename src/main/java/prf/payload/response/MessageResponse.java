package prf.payload.response;

public class MessageResponse {

	private String title;
	private String message;

	public MessageResponse(String message) {
		super();
		this.message = message;
	}
	
	public MessageResponse(String title,String message) {
		super();
		this.title=title;
		this.message = message;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	
	
}
