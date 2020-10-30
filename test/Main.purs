module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Jason.JasonSpec (jasonSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        jasonSpec
