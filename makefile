all: clean readme
	
readme: README.html

README.html: README.md
	pandoc --standalone --metadata title="README File" README.md -o README.html
	
clean:
	rm -rf ./README.html