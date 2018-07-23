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


# compile front end and move it to the resources folder for the api to serve
compile: compile-elm move-web-to-api
