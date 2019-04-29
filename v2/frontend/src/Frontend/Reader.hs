{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Reader where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad.Fix
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable

import Common.Api
import Common.Route
import Obelisk.Generated.Static

reader
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
reader = do
  mainContents
  pageChangeButtons

mainContents
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
mainContents = divClass "" $ do
  let
    style = "height" =: "15em"
      <> "writing-mode" =: "vertical-rl"
      -- <> "padding-top" =: "2em"
      <> "width" =: "80vw"
    attr = constDyn $
      "style" =:
      (T.intercalate " " $ map (\(k, v) -> k <> ": " <> v <> ";") $ Map.toList style)
    (p1, p2) = perRowContents 15 40 contents
  elDynAttr "div" attr $ for p1 $ \paraText ->
    el "p" $ text paraText
  elDynAttr "div" attr $ for p2 $ \paraText ->
    el "p" $ text paraText
  return ()

-- No of words per line W
-- No of total lines per row L
-- (Content to display on 1 row, rest of contents)
perRowContents :: Int -> Int -> [Text] -> ([Text], [Text])
perRowContents w l [] = ([], [])
perRowContents w l (c:cs) =
  if cl < l
    then (c:r2, e2)
    else ([c1], c2:cs)
  where
    cl = ceiling $ fromIntegral (T.length c) / (fromIntegral w)
    (r2, e2) = perRowContents w (l - cl) cs
    (c1, c2) = T.splitAt (w * l) c

pageChangeButtons
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
pageChangeButtons = do
  let
    attr = "class" =: "columns is-mobile"
      <> "style" =: "padding-top: 1em;"
  elAttr "div" attr $ do
    divClass "column" $ elClass "a" "button is-small is-fullwidth" $ text "<"
    divClass "column" $ elClass "a" "button is-small is-fullwidth" $ text ">"

contents =
  [ "平成最後の選挙となる統一地方選挙は、14日、政令指定都市以外の市と東京の特別区で、市区長と議員の選挙が告示され、後半戦がスタートしました。"
  , "４年に１度の統一地方選挙は、14日、政令指定都市以外の市と東京の特別区で、97の市区長選挙と、314の市と区の議員選挙が告示されました。"
  , "立候補の受け付けは午前８時半から各地の選挙管理委員会で行われていて、ＮＨＫのまとめによりますと、午後２時現在、97の市区長選挙には合わせて193人が立候補しました。"
  , "内訳を見ますと、現職が78人、元市長が３人、新人が112人です。女性は27人となっています。"
  , "県庁所在地では、水戸、津、高松、長崎、大分の５つの市長選挙が行われ、このうち、長崎市長選挙は、４期目を目指す現職と新人３人の合わせて４人が立候補し、８年ぶりの選挙戦となりました。"
  , "ー二三四五六七八九十ー二三四五六七八九十ー二三四五六七八九十ー二三四五六七八九十ー二三四五六七八九十ー二三四五六七八九十"
  , "一方、津と高松の２つの市長選挙は、これまでに現職のほかに立候補の届け出はなく、無投票となる公算が大きくなっています。"
  , "統一地方選挙の後半戦では、人口減少対策のほか、子育て支援や高齢者福祉など暮らしに身近なテーマをめぐって活発な論戦が交わされる見通しです。"
  , "市区長選挙や市と区の議会議員選挙は、衆議院の２つの補欠選挙や、16日に告示される町村長と町村議会議員の選挙とともに、今月21日に投票が行われます。"
  ]
