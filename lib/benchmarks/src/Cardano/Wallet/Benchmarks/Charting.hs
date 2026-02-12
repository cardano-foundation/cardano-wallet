module Cardano.Wallet.Benchmarks.Charting
    ( renderHarmonizedHistoryChartPng
    , renderHarmonizedHistoryChartSVG
    )
where

import Cardano.Wallet.Benchmarks.Collect
    ( Unit
    )
import Cardano.Wallet.Benchmarks.History
    ( HarmonizedHistory (..)
    , HarmonizedRow (..)
    , IndexedSemantic (IndexedSemantic)
    )
import Codec.Picture
    ( Image
    , PixelRGBA8
    , writePng
    )
import Control.Monad
    ( forM_
    , void
    )
import Data.Csv
    ( toField
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Data.Time
    ( Day
    )
import Diagrams.Backend.Rasterific
    ( Options (RasterificOptions)
    , Rasterific (..)
    )
import Diagrams.Prelude
    ( V2 (..)
    , dims
    , renderDia
    )
import Graphics.Rendering.Chart
    ( FontStyle (_font_size)
    , HTextAnchor (HTA_Centre)
    , Layout
    , LayoutPick
    , Renderable
    , VTextAnchor (VTA_Centre)
    , area_spots_fillcolour
    , area_spots_max_radius
    , area_spots_title
    , area_spots_values
    , fillBackground
    , label
    , laxis_generate
    , layoutToGrid
    , layout_y_axis
    , nullPickFn
    , render
    , scaledAxis
    , setPickFn
    , vectorAlignmentFns
    )
import Graphics.Rendering.Chart.Backend.Diagrams
    ( FileOptions (..)
    , defaultEnv
    , renderableToFile
    , runBackend
    )
import Graphics.Rendering.Chart.Easy
    ( Default (def)
    , blue
    , execEC
    , liftEC
    , line
    , plot
    , (.=)
    )
import Graphics.Rendering.Chart.Grid
    ( Grid
    , gridToRenderable
    , wideAbove
    )
import System.FilePath
    ( (<.>)
    , (</>)
    )
import Prelude

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Map.Monoidal.Strict as MMap

bsChart :: Set Day -> Map Day Double -> Layout Day Double
bsChart ds vs = execEC $ do
    layout_y_axis . laxis_generate .= scaledAxis def (0, maximum vs)
    let successes = Map.restrictKeys vs ds
    plot $ line "benchmark" $ pure $ Map.assocs successes
    plot $ liftEC $ do
        area_spots_title .= "value present"
        area_spots_fillcolour .= blue
        area_spots_max_radius .= 7
        area_spots_values
            .= fmap (\(d, v) -> (d, v, 1 :: Int)) (Map.assocs successes)

renderIndexedSemantic :: IndexedSemantic -> String
renderIndexedSemantic (IndexedSemantic s i) =
    B8.unpack (toField s) <> ", " <> show i

grid
    :: Set Day
    -> IndexedSemantic
    -> HarmonizedRow
    -> Grid (Renderable (LayoutPick Day Double Double))
grid ds is mr =
    let title :: Maybe Unit -> Renderable b
        title mu =
            setPickFn nullPickFn
                $ label font HTA_Centre VTA_Centre
                $ renderIndexedSemantic is
                    <> case mu of
                        Nothing -> ""
                        Just u ->
                            "( "
                                <> B8.unpack (toField (u :: Unit))
                                <> " )"
    in  case mr of
            Harmonized m u ->
                title (Just u) `wideAbove` layoutToGrid (bsChart ds m)
  where
    font = def{_font_size = 12}

renderHarmonizedHistoryChartSVG
    :: FilePath -> HarmonizedHistory -> IO ()
renderHarmonizedHistoryChartSVG dir (HarmonizedHistory h' ds) = do
    let mkSVGPath :: Int -> FilePath
        mkSVGPath n = dir </> "benchmarks-history-" <> show n <.> ".svg"
    forM_ (zip [1 :: Int ..] $ MMap.assocs h') $ \(n, (is, hr)) -> do
        let filePath = mkSVGPath n
        void
            $ renderableToFile
                def{_fo_size = (1500, 500)}
                (mkSVGPath n)
            $ fillBackground def
            $ gridToRenderable
            $ grid ds is hr
        putStrLn
            $ "Wrote: "
                <> filePath
                <> " with history of : "
                <> renderIndexedSemantic is

renderHarmonizedHistoryChartPng
    :: FilePath -> HarmonizedHistory -> IO ()
renderHarmonizedHistoryChartPng dir (HarmonizedHistory h' ds) = do
    let height = 500
        width = 1500
    d <- defaultEnv vectorAlignmentFns width height
    let imageOf :: Grid (Renderable a) -> (Image PixelRGBA8)
        imageOf grid' =
            renderDia Rasterific (RasterificOptions (dims (V2 width height)))
                $ fst
                $ runBackend d
                $ render
                    (fillBackground def $ gridToRenderable grid')
                    (width, height)
    let mkPNGPath :: Int -> FilePath
        mkPNGPath n = dir </> "benchmarks-history-" <> show n <.> ".png"
    forM_ (zip [1 :: Int ..] $ MMap.assocs h') $ \(n, (is, hr)) -> do
        let filePath = mkPNGPath n
        writePng filePath $ imageOf $ grid ds is hr
        putStrLn
            $ "Wrote: "
                <> filePath
                <> " with history of : "
                <> renderIndexedSemantic is
