echo "Setting up Project"

echo "***********************"
echo " Setting up Front End   "
echo "***********************"

if [ -d ./front-end ]; then
    echo "Looks like you already have a web directory"
else
	git clone git@github.com:simonh1000/elm-webpack-starter.git

	mv elm-webpack-starter front-end

	rm front-end/README.md
fi

echo "***********************"
echo " Setting up Back End   "
echo "***********************"

if [ -d ./back-end ]; then
    echo "Looks like you already have a back-end"
else
	git clone git@github.com:smaccoun/haskstar-haskell.git

	mv haskstar-haskell back-end
fi
