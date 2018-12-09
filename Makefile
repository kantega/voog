build:
	docker build -t voog .
	docker run -v $(shell pwd)/dist:/app/dist --rm voog
