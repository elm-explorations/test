- [ ] elm-test
          --line-coverage lcov|html|csv|plaintext|stdout
          --line-coverage-output ./coverage/foo.txt
      [RUNNER]

      Turns line coverage wrapper over testing on:
      - instrument Elm files
      - run whatever elm-test did, on the instrumented files instead of the originals
      - after tests finish, read the coverage counts from a JS global
      - report (via `elm-test coverage-report`) in the correct format

- [ ] elm-test coverage-instrument
          File.elm+
          [--output ./instrumented]
      [RUNNER]

      Makes a directory with the specified files instrumented.

      By default it goes in elm-stuff/instrumented.
      If using the default and the path exists, asks before overwriting.

      Part of the output is coverage-metadata.json, containing function
      names, source code regions and <TODO-FINISH> of the corresponding pointIds
      (Dict Int {functionName: String, region: ((Int,Int),(Int,Int))} in spirit).

- [ ] Coverage.enableLineCoverage ()
      [LIBRARY]

      No-op (patched in the compiled JS file with elm-test instrument-patch).

      When patched, initializes a counter (Dict Int Int, in spirit).

- [ ] Coverage.track pointId
      [LIBRARY]

      No-op (patched in the compiled JS file with elm-test instrument-patch).

      When patched, increments counter for a given pointId.

- [ ] elm-test coverage-patch compiled.js
          [--output patched.js]
      [RUNNER]

      Switches Coverage.* functions from no-ops to their effectful variants.

- [ ] elm-test coverage-report
          counter.json
          --metadata coverage-metadata.json
          [--format lcov|html|csv|plaintext|stdout]
          [--output path]
      [RUNNER]

      Outputs the JSON data in the requested format.

      By default it goes in elm-stuff/coverage-YYYY-MM-DD-HH-MM-SS.EXT
      (or nowhere if the format is stdout).

      HTML path needs to be a folder (`index.html` for the summary
      and one HTML file per instrumented Elm file, with folder paths included).
