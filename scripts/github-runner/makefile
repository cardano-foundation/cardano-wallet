deploy: 
	GITHUB_TOKEN=`cat /secrets/github-token` RUNNER_NAME=paolino NIX_VOLUME=/nix-ci docker-compose up -d

kill:
	docker-compose down 
	docker-compose rm

logs:
	docker-compose logs -ft 
