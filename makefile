ROOT = ~/Development/tudu
WEB = $(ROOT)/web/src
WEBOUT = $(ROOT)/web/build/public/js/elm.js
API = $(ROOT)/api

develop:
	foreman start

## FRONTEND

compile-elm:
	 cd $(WEB); elm-make main.elm --yes --warn --output\=elm.js; mv elm.js $(WEBOUT) ; cd $(ROOT) 

trash-web:
	trash $(WEBOUT)/public/js/elm.js; trash $(API)/resources/public/build


## SERVER

move-web-to-api:
	cd $(ROOT); cp -R web/build/ api/resources/public

run-api:

	cd $(API); lein ring server 


# Combined Tasks.

compile: compile-elm move-web-to-api
