default: dirs

dirs:
	mkdir -p root/htdocs/assets
	mkdir -p public/
	mkdir -p tmp logs
	cp image-not-available*.png root/htdocs/assets

demo: dirs
	unzip sample-images.zip

clean: 
	rm -rf root/htdocs logs tmp

release:
	raco exe movies.rkt

ssl: private-key.pem server-cert.pem

private-key.pem:
	openssl genrsa -out private-key.pem 2048

server-cert.pem: private-key.pem
	openssl req -new -x509 -nodes -sha1 -days 365 -key private-key.pem \
		> server-cert.pem
