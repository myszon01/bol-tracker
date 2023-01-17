# bol-tracker
This is pet project to demonstrate and learn how to write smart contracts on Cardano blockchain

![Alt text](/img/project.png "Smart contract")

# Running

1. Make sure you have [nix](https://github.com/input-output-hk/plutus/blob/master/CONTRIBUTING.adoc#installing-and-setting-up-nix) setup 
2. Pull [plutus-apps](https://github.com/input-output-hk/plutus-apps/tree/v1.1.0) annd checkout to `v1.1.0`
3. From root of plutus-apps run `nix-shell`
4. Once in nix shell navigate to bol-tracker project root directory 
5. Run `cabal update` next `cabal build` and finally `cabal repl`
6. Next you need to load test module by running `:l TestBoLTracker`
7. To execute sample transactions run `runContract AllGood` to test happy path or `runContract Rejected` to test alternative branch