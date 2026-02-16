You will need the following:

- Contributor access to `elm-explorations/test`

How to publish a release:

1. Check out the `master` branch and make sure it is up-to-date.
1. Run `npx elm diff` and make sure the version change is what you expect.
1. Run `npx elm bump` (updating the version number in `./elm.json`).
1. Update `./tests/elm.json` and `./package.json` to use the new version number.
1. Update `./CHANGELOG.md` to add notes about the new version.
1. Make sure the tests pass: `npm install && npm run test`
1. Commit the changes to `./elm.json`, `./tests/elm.json`, `./package.json` and `./CHANGELOG.md`.
1. Run `npx elm publish` -- it should fail complaining that the version is not tagged.
1. Tag and push the release using the git commands that elm publish suggested.
1. Run `npx elm publish` to publish the release.
