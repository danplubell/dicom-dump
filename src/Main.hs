{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.ByteString.Char8 as BSC
import           Data.DICOM.Dictionary
import           Data.DICOM.Model
import           Data.DICOM.Parse
import           System.Environment
import           Text.Printf
main :: IO ()
main = do
  (fp:_) <- getArgs
  let dd = loadElementDictionary
  fc  <- parseDicomFile fp
  case fc of
    Left s -> putStrLn $ "A parsing error occured: " ++ s
    Right elements -> mapM_ (pPrintf dd) elements

pPrintf::PrintfType t =>DicomDictionary -> DataElement -> t
pPrintf dd de =
  case de of
    Element {..} -> printf "(%04x,%04x) %s % 10d %- 40s %- 20s\n" (fst deTag ) (snd deTag ) (show deVR ) deVL (BSC.unpack $ lookupEleName deTag) ((show . BSC.take 40) deRawValue)
    Item {..}    -> printf "(%04x,%04x) na % 10d %- 40s\n" (fst diTag ) (snd diTag ) diVL (BSC.unpack $ lookupEleName diTag)
  where lookupEleName t = maybe "Unknown Tag and Data" name (lookupElementByTag dd t)

