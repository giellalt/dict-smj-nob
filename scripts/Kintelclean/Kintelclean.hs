{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core
import Data.List.Split
import System.Environment
import System.Exit
import Text.Regex.Posix ((=~))
import Text.Regex (subRegex, mkRegex)
import Text.Printf (printf)
-- import Text.XML.HXT.XPath


-- | XML helpers:
isTag :: ArrowXml cat => String -> cat XmlTree XmlTree
isTag tag = isElem >>> hasName tag

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

notNamed :: ArrowXml a => String -> a XmlTree XmlTree
notNamed n = (getName >>> isA (/= n)) `guards` this

parentOf :: ArrowXml a => String -> a XmlTree XmlTree
parentOf tag = isElem /> isTag tag

replaceChildrenNamed :: ArrowXml a => String -> a XmlTree XmlTree -> a XmlTree XmlTree
replaceChildrenNamed tag replacements =
  replaceChildren ((getChildren >>> (notNamed tag))
                   <+>
                   replacements)

type Atts = [(QName, String)]

mk_atts :: ArrowXml a => Atts -> [a n XmlTree]
mk_atts atts = map f atts
  where f (n, v) = sqattr n v

get_atts :: ArrowXml a => a XmlTree Atts
get_atts = proc x -> do
    a <- listA (((getAttrName &&& text)) <<< getAttrl) -< x
    returnA -< a


-- | This is where it happens:
splitElts :: ArrowXml a => a XmlTree XmlTree
splitElts =
  processTopDown ((splitMgs `when` (parentOf "mg" //> hasName "t"))
                  >>>
                  (splitTs `when` (parentOf "t")))
  where
    splitMgs = replaceChildrenNamed "mg" newMgs
    splitTs = replaceChildrenNamed "t" newTs


newMgs :: ArrowXml a => a XmlTree XmlTree
newMgs = proc parent -> do
  old_mg <- (this /> hasName "mg") -< parent
  (atts, trans) <- (this //> hasName "t" >>> (get_atts &&& text)) -< old_mg
  let tparts = splitTranslationOn ";" trans
  let t_elts = map (newT atts) tparts
  let new_mgs = map (newMg old_mg) t_elts
  foldl1 (<+>) new_mgs -<< ()

  where
    newT t_atts t_text =
      mkelem "t" (mk_atts t_atts) [txt t_text] -- see newTs for cnsGrd fixes
    newMg old_mg t_elt =
      constA old_mg
      >>>
      processTopDown (replaceChildrenNamed "t" t_elt `when` parentOf "t")


newTs :: ArrowXml a => a XmlTree XmlTree
newTs = proc parent -> do
  (atts, trans) <- (this //> hasName "t" >>> (get_atts &&& text)) -< parent
  let tparts = splitTranslationOn "," trans
  let t_elts = map (newT atts) tparts
  foldl1 (<+>) t_elts -<< ()

  where
    newT t_atts t_text =
      -- Move stem consonant into an attribute:
      let (text_no_grd, cnsGrd_atts) = cnsGrdToAtts t_text
          -- Fix the word count attribute:
          atts' = fixWC text_no_grd t_atts ++ cnsGrd_atts
      in
       mkelem "t" (mk_atts atts') [txt text_no_grd]



-- | String splitting and attribute fixes:

-- TODO: the ones that split on «el.» are a bit more difficult,
-- typically some ellipsis happening there :-/
splitTranslationOn :: String -> String -> [String]
splitTranslationOn delims s =
  map trim $ split (dropDelims . dropBlanks $ oneOf delims) s


trim :: String -> String
trim = unwords . words

fixWC :: String -> Atts -> Atts
fixWC t =
  let w_cnt = show . length $ words t
      txtwCntAtt = mkName "txtw_cnt"
  in
   map (\(n,v) -> if n == txtwCntAtt
                  then (txtwCntAtt, w_cnt)
                  else (n, v))

{-
Some entries have e.g. <l pos="N" fincons="ss" todo_3="s">adnonjuolgadus</l>
but we ignore this here, it belongs in monolingual analyser

TODO: some have attributes like 'todo="ht"' (looks like a cnsGrd?)
      probably also cnsGrd: <t info="l'l">buolle</t>
      might want to rename to cg for consistency?

The cg attribute has a lot of unstructured info:
  rg* means the strong grade is rg, but there's a vowel change as well
  f'f:ff means the strong grade is three, but spelt as ff
-}
cnsGrdToAtts :: String -> (String, Atts)
cnsGrdToAtts t_text =
  let (text', cnsGrd, _) = t_text =~ cnsGrdPat :: (String,String,String)
      cnsGrd_atts = case cnsGrd of
                    "" -> []
                    _ -> [(cnsGrdAtt, cgFormat cnsGrd)]
      -- TODO: what if cg already in atts? (does that happen?)
  in
   (trim text', cnsGrd_atts)
  where
    strongGrd = printf "\\(%s'%s\\)" cns2 cns2 :: String
    cnsGrdPat = printf "( %s+| %s| %s{2,}[*]?| -%s+-)*$" cns1 strongGrd cns2 letter :: String
    cns1 = "[bDdfGgjlmnŋprsVvbd]"
    cns2 = "[bDdfGgjlmnŋprsVvbdthkRVSJN]"
    letter = "(\\w|æ|ø|å|á|Æ|Ø|Å|Á)" -- very odd bug, ŋ matched by \w but not á??
    cnsGrdAtt = mkName "cg"
    -- Turn "(f'f) ff" into "f'f:ff"
    -- (well, "f&apos;f:ff", but that's probably OK …)
    cgFormat cg = subRegex (mkRegex strongGrd') (trim cg) "\\1:"
      where strongGrd' = printf "\\((%s'%s)\\) *" cns2 cns2 :: String






-- | Startup boilerplate:
process	:: String -> String -> IOSArrow b Int
process src dst =
  configSysVars [withValidate no, withIndent yes, withInputEncoding utf8]
  >>> readDocument [] src
  >>> splitElts
  >>> writeDocument [] dst
  >>> getErrStatus

main :: IO ()
main = do
  [src, dst] <- getArgs
  [rc] <- runX $ process src dst
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess

-- Test:
test :: IO ()
test = runX (readDocument [withValidate no] "test.xml"
             >>> splitElts
             >>> writeDocumentToString [withIndent yes])
       >>= putStrLn . head

