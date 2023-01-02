package prf.services;

import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring4.SpringTemplateEngine;

import prf.entities.User;
import prf.repositories.UserRepository;

@Service
public class NotificationServicesImpl implements INotificationServices{

	private static final Logger log = Logger.getLogger(NotificationServicesImpl.class);
	
	private static Random rnd = new Random();
	
	@Autowired
    private SpringTemplateEngine templateEngine;

    @Autowired
    private JavaMailSender sender;
    
    @Autowired
    private UserRepository userRepository;

    @Override
    public void sendMailWithCode(User user,Boolean withCode) throws MessagingException {

        MimeMessage message = sender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(message,
                MimeMessageHelper.MULTIPART_MODE_MIXED_RELATED,
                StandardCharsets.UTF_8.name());

        Map<String, Object> model = new HashMap<>();
        model.put("name",user.getFirstname() + " "+user.getLastname());
        if(Boolean.TRUE.equals(withCode)) {
        	String code  = this.getRandomNumber(6);
        	user.setCode(Integer.parseInt(code));
        	user.setCodeExpiryDate(this.getDate(24));
        	model.put("code",code);
        }

        Context context = new Context();
        context.setVariables(model);
        String html = "";
        if(Boolean.TRUE.equals(withCode)) {
        	html = templateEngine.process("code_validation", context);
        }else {
        	html = templateEngine.process("email_confirmation", context);
        }

        try {
            helper.setTo(user.getEmail());
            helper.setText(html,true);
            if(Boolean.TRUE.equals(withCode)) {
            	helper.setSubject("Code de validation");
            }else {
            	helper.setSubject("Confirmation de Mail");
            }
        } catch (javax.mail.MessagingException e) {
            log.debug(e);
        }
        userRepository.save(user);
        sender.send(message);
    }
    
    private String getRandomNumber(int digCount) {
        StringBuilder sb = new StringBuilder(digCount);
        for(int i=0; i < digCount; i++)
            sb.append((char)('0' + rnd.nextInt(10)));
        return sb.toString();
    }

	@Override
	public void notifiyPersonnale(User user,String password) throws MessagingException {
		MimeMessage message = sender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(message,
                MimeMessageHelper.MULTIPART_MODE_MIXED_RELATED,
                StandardCharsets.UTF_8.name());

        Map<String, Object> model = new HashMap<>();
        model.put("name",user.getFirstname() + " "+user.getLastname());
        model.put("username",user.getEmail());
        model.put("password",password);

        Context context = new Context();
        context.setVariables(model);
        String html = "";
       
        html = templateEngine.process("personnals_notification", context);
        

        try {
            helper.setTo(user.getEmail());
            helper.setText(html,true);
            helper.setSubject("Notification au personnel");
        } catch (javax.mail.MessagingException e) {
            log.debug(e);
        }
        userRepository.save(user);
        sender.send(message);
	}
	
	private Date getDate(int addindNombre) {
		Date newDate = null;
		
		Calendar cal = Calendar.getInstance(); 
		cal.add(Calendar.HOUR, addindNombre);
		
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 
		//Date après avoir ajouté les années à la date indiquée
		String echeance = sdf.format(cal.getTime());
		try {
			newDate = sdf.parse(echeance);
		} catch (ParseException e) {
			e.printStackTrace();
		}
		return newDate;
	}
}
