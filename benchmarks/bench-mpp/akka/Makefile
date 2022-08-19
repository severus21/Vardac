default: build 


build_main:
	gradle -x test jarMain jarInstrumentationAgent 

build_pingService:
	gradle -x test jarPingService jarInstrumentationAgent

build_pongService:
	gradle -x test jarPongService jarInstrumentationAgent

build: protobuf  build_main build_pingService build_pongService

.PHONY:protobuf
protobuf:
	gradle wrapper
	./gradlew build -x test

.PHONY: test
test:
	gradle test