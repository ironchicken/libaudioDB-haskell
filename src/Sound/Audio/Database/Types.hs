-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
-- Author: richard.lewis@gold.ac.uk

-- This file is part of libaudioDB-haskell.

-- libaudioDB-haskell is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- libaudioDB-haskell is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with libaudioDB-haskell. If not, see <http://www.gnu.org/licenses/>.

module Sound.Audio.Database.Types ( Frame
                                  , Seconds
                                  , FrameSize
                                  , FeatureRate
                                  , inSeconds
                                  , inFrames
                                  , withSeconds
                                  , withFrames ) where

type Frame = Int
type Seconds = Double
type FrameSize = (Frame -> Seconds)
type FeatureRate = (Seconds -> Frame)

inSeconds :: FrameSize
inSeconds = fromIntegral

inFrames :: FeatureRate
inFrames = ceiling

withSeconds :: FeatureRate -> FrameSize -> (Seconds -> Seconds) -> Frame -> Frame
withSeconds secToFrames framesToSec f frames = (secToFrames . f . framesToSec) frames

withFrames :: FeatureRate -> FrameSize -> (Frame -> Frame) -> Seconds -> Seconds
withFrames secToFrames framesToSec f seconds = (framesToSec . f . secToFrames) seconds
