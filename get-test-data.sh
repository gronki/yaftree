if [ ! -f english30k.txt ]; then
	curl -L https://github.com/arstgit/high-frequency-vocabulary/raw/refs/heads/master/30k.txt -o english30k.txt
fi
