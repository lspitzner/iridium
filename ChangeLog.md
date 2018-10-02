# Revision history for iridium

## 0.1.5.8  -- 2018-10-02

  * Add support for -Wcompat
  * Reduce dependency footprint by using json endpoint of hackage interface

## 0.1.5.7  -- 2017-08-01

  * Adapt for cabal-2.0 - Note that this is a breaking change; uploading with
    cabal-1.* will not work with this version.

## 0.1.5.6  -- 2016-09-28

  * Only internal changes (support ghc-8, fix support for ghc-7.8;
    adapt/switch to turtle-1.3.0)

## 0.1.5.5  -- 2016-09-28

  * Slight improvements around the handling of testing against multiple
    compiler versions

## 0.1.5.4  -- 2016-05-21

  * Fix git branch parsing issue

## 0.1.5.3  -- 2016-04-22

  * Include `cabal update` invocation by default
  * Add warning for missing remote version

## 0.1.5.2  -- 2016-03-11

  * Improve stackage upper-bound check error output

## 0.1.5.1  -- 2016-03-11

  * Fix iridium package pvp compliance (lower bounds)
  * First hackage release

## 0.1.5.0  -- 2016-03-11

  * Add package-sdist check

  * Prepare non-static default config

## 0.1.4.0  -- 2016-02-22

  * Fix various bugs

  * Make various changes to the default iridium.yaml

  * Fix/Expand basic git functionality; it includes:
    * Displaying current branch
    * Tagging the current commit
    * Pushing tag and branch to remote

## 0.1.4.0  -- 2016-02-18

  * Start integrating some git-specific functionality

## 0.1.2.0  -- 2016-02-17

  * First release, experimental.
