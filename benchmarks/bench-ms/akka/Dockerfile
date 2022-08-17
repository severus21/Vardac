FROM gradle:7.1.1-jdk11 AS build
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle wrapper  && ./gradlew build -x test && gradle -x test jarMain 

FROM openjdk:11.0.13-jre-buster
RUN wget https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh -O /bin/wait-for-it.sh && chmod u+x /bin/wait-for-it.sh
COPY --from=build /home/gradle/src/build/libs/* /bin/kvs/
WORKDIR /bin/kvs
EXPOSE 25520/tcp
EXPOSE 8080/tcp
ENTRYPOINT [ "/bin/bash", "-l", "-c" ]