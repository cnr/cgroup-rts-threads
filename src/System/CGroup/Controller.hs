-- | Common types and operations for CGroup controllers.
module System.CGroup.Controller (
  -- * CGroup Controllers
  Controller (..),
  resolveCGroupController,
  resolveCGroupController',
) where

import System.CGroup.Controller.Internal (Controller (..), resolveCGroupController, resolveCGroupController')
