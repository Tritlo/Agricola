

serve: build
	.cabal-sandbox/bin/warp -d src
	sleep 3

autoreload:
	sleep 3 && xdotool search --onlyvisible -class "Chromium-browser" windowfocus key --window %@ 'ctrl+r' 1>&2 &
	sleep 3 && xdotool search --onlyvisible -class "google-chrome" windowfocus key --window %@ 'ctrl+r' 1>&2 &
	make serve

watch:
	ls src/Agricola.hs | entr -r make autoreload

build: boothaste
	.cabal-sandbox/bin/hastec -Wall -fno-warn-unused-do-bind src/Agricola.hs
	rm src/Agricola.o

boothaste: sandbox
	.cabal-sandbox/bin/haste-boot

sandbox:
	cabal sandbox init
	cabal install --only-dependencies
