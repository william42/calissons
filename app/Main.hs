{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (steps,dia,padAll,main) where

import Lib
import Diagrams.Prelude hiding (start)
import Diagrams.Backend.Rasterific
import Control.Monad.Random.Lazy
import Control.Monad.Random.Class
import Codec.Picture.ColorQuant
import Web.Hastodon
import Dhall
import qualified Data.Text.Lazy as T

import           Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy                as L (writeFile)

cfgFile = "~/.calissons"
filename = "/tmp/calissons.gif"

data HastodonConfig = HastodonConfig { clientId :: Text,
                       clientSecret :: Text,
                       username :: Text,
                       password :: Text,
                       server :: Text} deriving Generic

instance Interpret HastodonConfig

mkHastodonClientFromConfig :: HastodonConfig -> IO (Maybe HastodonClient)
mkHastodonClientFromConfig (HastodonConfig cId cSecret un pw serv) =
  mkHastodonClient (T.unpack cId) (T.unpack cSecret) (T.unpack un) (T.unpack pw) (T.unpack serv)


iterateR :: (a -> Rand StdGen a) -> a -> Rand StdGen [a]
iterateR f x0 = do
  x1 <- f x0
  (x0:) <$> iterateR f x1

steps :: Rand StdGen [TD]
steps = iterateR stepRandom start

steps' :: Int -> Int -> Int -> Rand StdGen [TD]
steps' l m n = iterateR stepRandom' (start' l m n)

dia :: TD -> Diagram B
dia = diaTD

padAll :: [Diagram B] -> [Diagram B]
padAll x = map (withEnvelope y) x
  where y = mconcat x

main :: IO ()
--main = (padAll . map dia <$> sequence (take 10 $ tail steps)) >>= uniformGifMain 10

toot a = "A chain of cubes from " ++ (show . volume . head) a ++ " cubes to " ++ (show . volume . last) a ++ " cubes."

-- my code doesn't require lots of colors and I think the sophisticated shit actually has bugs
calissonsPaletteOptions = PaletteOptions
  { paletteCreationMethod = Uniform,
    enableImageDithering = False,
    paletteColorCount = 256 }

-- using rasterGif isn't optimal but I'll send bug reports later
calissonsGif dias = let diasWithTime = [(d,15)|d <- dias] in
  case rasterGif (dims2D 1000 1000) LoopingForever calissonsPaletteOptions diasWithTime of
  Right bs -> L.writeFile filename bs
  Left e -> putStrLn e



sideLen = uniform [2..8]

main = do
  a1 <- evalRandIO steps
  l <- sideLen
  m <- sideLen
  n <- sideLen
  a2 <- evalRandIO $ steps' l m n
  aInf <- uniform [a1,a2]
  k <- uniform [0,0,100,10000]
  let a = take 100 $ drop k aInf
  putStrLn "Steps ready!"
  putStrLn $ (show . volume . head) a ++ " to " ++ (show . volume . last) a ++ "."
  let b = padAll $ map dia a
  putStrLn "Diagrams ready!"
  calissonsGif b
  putStrLn "GIF created!"
  cfg <- input auto cfgFile
  maybeClient <- mkHastodonClientFromConfig cfg
  case maybeClient of
    Just client -> do
      attRes <- postMediaFile client filename "Steps of a Markov chain on plane partitions."
      case attRes of
        Left _ -> putStrLn "Failed to upload file."
        Right attachment -> do
          let calId = attachmentId attachment
          result <- postStatusWithMediaIds client (toot a) [calId]
          putStrLn "Tooted successfully! (probably)"
    Nothing -> putStrLn "Failed to connect to Mastodon."

