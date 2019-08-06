IMAGENAME=romezzz/nvim
USER = "$(shell id -u):$(shell id -g)"

build:
	docker build -t $(IMAGENAME) .

bash:
	docker run -v `pwd`:/src -it romezzz/nvim bash

run:
	docker run --volume `pwd`:/src -it romezzz/nvim nvim

push:
	docker push $(IMAGENAME)

nvim-build:
	  docker-compose build

nvim-bash:
	docker-compose run nvim bash
