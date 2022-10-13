default: build 


build_main:
	gradle -x test jarMain jarInstrumentationAgent 

build_lmain:
	gradle -x test jarLMain jarInstrumentationAgent 

build_clientService:
	gradle -x test jarGetRequestService jarInstrumentationAgent

build_shardService:
	gradle -x test jarGetResultService jarInstrumentationAgent

build: protobuf  build_main build_lmain build_clientService build_shardService

.PHONY:protobuf
protobuf:
	gradle wrapper
	./gradlew build -x test

.PHONY: test
test:
	gradle test