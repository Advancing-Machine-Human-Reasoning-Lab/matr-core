# matr makefile

PROJDIR := $(realpath $(CURDIR))
SOURCEDIR := $(PROJDIR)/src
# Directory for *.clj source files
CLJ_SOURCEDIR := $(SOURCEDIR)/matr_core
# Directory for *.cljs source files
CLJS_SOURCEDIR := $(SOURCEDIR)/matr_gui_clj
RESOURCEDIR := $(PROJDIR)/resources
CSSDIR := $(RESOURCEDIR)/public/css
IMAGEDIR := $(RESOURCEDIR)/public/images
JSDIR := $(RESOURCEDIR)/public/js

# List of *.clj source files
CLJ_SOURCES   = $(wildcard $(CLJ_SOURCEDIR)/*.clj)
# List of *.cljs source files
CLJS_SOURCES  = $(wildcard $(CLJS_SOURCEDIR)/*.cljs)
# List of *.cljc source files
CLJC_SOURCES  = $(wildcard $(CLJ_SOURCEDIR)/*.cljc)
HTML_DEPS = $(RESOURCEDIR)/kioo_template.html $(RESOURCEDIR)/public/index.html
CSS_DEPS  = $(wildcard $(CSSDIR)/*.css)
JS_DEPS  = $(wildcard $(JSDIR)/*.js)
# List of *.png, *.jpg, *.jpeg, *.gif files in resources
IMAGE_DEPS  = $(wildcard $(IMAGEDIR)/*.png) $(wildcard $(IMAGEDIR)/*.jpg) $(wildcard $(IMAGEDIR)/*.jpeg) $(wildcard $(IMAGEDIR)/*.gif)

# Dependencies for matr_gui_clj.js
FRONTEND_DEPS = $(CLJS_SOURCES) $(CLJC_SOURCES) $(HTML_DEPS) $(CSS_DEPS) $(JS_DEPS) $(IMAGE_DEPS)

JS_TARGET = resources/public/js/compiled/matr_gui_clj.js
UBERJAR = target/uberjar/matr.jar

$(UBERJAR): $(CLJ_SOURCES) $(JS_TARGET) pom.xml
	clj -X:uberjar

pom.xml: deps.edn
	clj -Spom

$(JS_TARGET): $(FRONTEND_DEPS)
	clj -M:dev:min
# end
