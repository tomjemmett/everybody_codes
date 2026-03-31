# Everybody Codes

This is my attempt at solving Everybody Codes, in [Haskell](https://www.haskell.org/).

## How to run

- `cabal run ec2025` will run all of the quests for 2025.
- `cabal run ec2025 quest_NUMBER` will run the code for the give quest.
- `cabal test` will run the test suite, ensuring that the results are correct (first against the provided sample, then against the actual result).
- `cabal test spc --test-options "--match Events.Y2025` will test just the 2025 event

Replace `ec2025` with `ec2024`, `story1`, `story2`, or `story3`.
