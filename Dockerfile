FROM openjdk:8
VOLUME /tmp
EXPOSE 8086
ADD papillonRfashion-1.1.0-SNAPSHOT.jar papillonRfashion.jar
ENTRYPOINT ["java","-jar","papillonRfashion.jar"]