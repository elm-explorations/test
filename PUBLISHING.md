
You will need the following:

- Contributor access to `elm-explorations/test`
- Able to run `runhaskell` from the command line

How to publish a release:

1. Check out the `master` branch and make sure it is up-to-date.
1. Run `npx elm diff` and make sure the version change is what you expect.
1. Run `npx elm bump`.
1. Update `./tests/elm.json`, `./tests/test.sh`, and `./tests/MakeTestRegistry.hs` to use the new version number.
1. Make sure the tests run `npm install && npm test`
1. Running the tests should have modified `./tests/versions.dat` (if not, you need to install Haskell such that you have `runhaskell` on your path)
1. Commit the changes to `./elm.json`, `./tests/elm.json`, `./tests/test.sh`, `./tests/MakeTestRegistry.hs`, and `./tests/versions.dat`
1. Run `npx elm publish` -- it should fail complaining that the version is not tagged.
1. Tag and push the release using the git commands that elm publish suggested.
1. Run `npx elm publish` to publish the release.
