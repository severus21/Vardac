default: build 


build_main:
	gradle -x test jarMain 

build_pingService:
	gradle -x test jarPingService

build_pongService:
	gradle -x test jarPongService

build: protobuf  build_main build_pingService build_pongService

.PHONY:protobuf
protobuf:
	gradle wrapper
	./gradlew build -x test

.PHONY: test
test:
	gradle test