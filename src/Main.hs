{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BL
import           Data.DICOM.Dictionary
import           Data.DICOM.Model
import           Data.DICOM.Parse
import           System.Environment
import           System.IO
import           Text.Printf
main :: IO ()
main = do
  (fp:_) <- getArgs
  h <- openBinaryFile fp ReadMode
  contents <- BL.hGetContents h
  let dd = loadElementDictionary
  let fc = parseDicomFileContent contents
  mapM_ (pPrintf dd) fc

pPrintf::PrintfType t =>DicomDictionary -> DataElement -> t
pPrintf dd de =
  case de of
    Element {..} -> printf "(%04x,%04x) %s % 10d %s\n" (fst deTag ) (snd deTag ) (show deVR ) deVL (BSC.unpack $ lookupEleName deTag)
    Item {..}    -> printf "(%04x,%04x) na % 10d %s\n" (fst diTag ) (snd diTag ) diVL (BSC.unpack $ lookupEleName diTag)
  where lookupEleName t = maybe "Unknown Tag and Data" name (lookupElementByTag dd t)

