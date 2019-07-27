IMAGENAME=romezzz/nvim

build:
	docker build -t $(IMAGENAME) .

bash:
	docker run --volume `pwd`:/src -it romezzz/nvim bash

run:
	docker run --volume `pwd`:/src -it romezzz/nvim nvim

push:
	docker push $(IMAGENAME)

