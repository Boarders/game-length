build:=cabal new-
cabalfile:=game-length.cabal

build: ${cabalfile} cabal-setup
build:
	${build}build && ${build}run length

cabal-setup: ${cabalfile}
cabal-setup:
	cabal new-configure --project-file=cabal.project
