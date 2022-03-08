all: clean readme
	
readme: README.html

README.html: README.md
	quarto render README.md
	
clean:
	rm -rf ./README.html