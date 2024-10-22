module Cardano.Wallet.UI.Lib.Pagination.MapSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.UI.Lib.Pagination.Map
    ( mkStrictMapPaginate
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( MkPaginatePure
    , Paginate (..)
    , PaginatePure
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Strict
    ( Map
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

import qualified Data.Map.Strict as Map

listPaginate :: MkPaginatePure Int [Int]
listPaginate pageSize xs =
    Paginate
        { nextIndex = \k ->
            let
                dk = length $ drop k xs
            in
                if dk > pageSize then Just (k + pageSize) else Nothing
        , previousIndex = \k ->
            let
                dk = length $ take k xs
            in
                if dk >= pageSize then Just (k - pageSize) else Nothing
        , pageAtIndex = \k ->
            let
                r = take pageSize $ drop k xs
            in
                if null r
                    then Nothing
                    else
                        Just (length r, r)
        , minIndex = case xs of
            [] -> Nothing
            (_x : _) -> Just 0
        }

matchPaginate
    :: Int
    -> PaginatePure Int [Int]
    -> PaginatePure Int (Map Int Int)
    -> Spec
matchPaginate k listPaginated mapPaginated =
    let
        listNext = nextIndex listPaginated k
        mapNext = nextIndex mapPaginated k
        listPrevious = previousIndex listPaginated k
        mapPrevious = previousIndex mapPaginated k
        listPage = pageAtIndex listPaginated k
        mapPage = pageAtIndex mapPaginated k
    in
        do
            it "matches nextIndex"
                $ listNext
                `shouldBe` mapNext
            it "matches previousIndex"
                $ listPrevious
                `shouldBe` mapPrevious
            it "matches pageAtIndex"
                $ listPage
                `shouldBe` fmap (fmap toList) mapPage
            it "matches minIndex"
                $ minIndex listPaginated
                `shouldBe` minIndex mapPaginated

spec :: Spec
spec = do
    describe "list pagination" $ do
        it "works on empty lists" $ do
            let paginated = listPaginate 10 []
            minIndex paginated `shouldBe` Nothing
            pageAtIndex paginated 0 `shouldBe` Nothing
            nextIndex paginated 0 `shouldBe` Nothing
            previousIndex paginated 0 `shouldBe` Nothing
        it "works on lists with less than a page" $ do
            let paginated = listPaginate 10 [1, 2, 3]
            minIndex paginated `shouldBe` Just 0
            pageAtIndex paginated 0 `shouldBe` Just (3, [1, 2, 3])
            nextIndex paginated 0 `shouldBe` Nothing
            previousIndex paginated 0 `shouldBe` Nothing
        it "works on lists with exactly a page" $ do
            let paginated = listPaginate 10 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            minIndex paginated `shouldBe` Just 0
            pageAtIndex paginated 0
                `shouldBe` Just (10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
            nextIndex paginated 0 `shouldBe` Nothing
            previousIndex paginated 0 `shouldBe` Nothing
        it "works on lists with more than a page" $ do
            let paginated = listPaginate 10 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
            minIndex paginated `shouldBe` Just 0
            pageAtIndex paginated 0
                `shouldBe` Just (10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
            nextIndex paginated 0 `shouldBe` Just 10
            previousIndex paginated 0 `shouldBe` Nothing
        it "respects next-previous symmetry" $ do
            let paginated = listPaginate 10 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
            minIndex paginated `shouldBe` Just 0
            nextIndex paginated 0 `shouldBe` Just 10
            previousIndex paginated 10 `shouldBe` Just 0
    describe "strict map pagination" $ do
        describe "works on empty maps" $ do
            let paginatedMap = mkStrictMapPaginate 10 (mempty :: Map Int Int)
            let paginatedList = listPaginate 10 []
            matchPaginate 0 paginatedList paginatedMap
        describe "works on maps with less than a page" $ do
            let paginatedMap =
                    mkStrictMapPaginate 10
                        $ Map.fromList [(0, 1), (1, 2), (2, 3)]
            let paginatedList = listPaginate 10 [1, 2, 3]
            matchPaginate 0 paginatedList paginatedMap
        describe "works on maps with exactly a page" $ do
            let paginatedMap =
                    mkStrictMapPaginate 10
                        $ Map.fromList
                            [ (0, 1)
                            , (1, 2)
                            , (2, 3)
                            , (3, 4)
                            , (4, 5)
                            , (5, 6)
                            , (6, 7)
                            , (7, 8)
                            , (8, 9)
                            , (9, 10)
                            ]
            let paginatedList = listPaginate 10 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            matchPaginate 0 paginatedList paginatedMap
        describe "works on maps with more than a page" $ do
            let paginatedMap =
                    mkStrictMapPaginate 10
                        $ Map.fromList
                            [ (0, 1)
                            , (1, 2)
                            , (2, 3)
                            , (3, 4)
                            , (4, 5)
                            , (5, 6)
                            , (6, 7)
                            , (7, 8)
                            , (8, 9)
                            , (9, 10)
                            , (10, 11)
                            ]
            let paginatedList =
                    listPaginate
                        10
                        [ 1
                        , 2
                        , 3
                        , 4
                        , 5
                        , 6
                        , 7
                        , 8
                        , 9
                        , 10
                        , 11
                        ]
            matchPaginate 0 paginatedList paginatedMap
            matchPaginate 10 paginatedList paginatedMap
        it "respects next-previous symmetry" $ do
            let paginatedMap =
                    mkStrictMapPaginate 10
                        $ Map.fromList
                            [ (0 :: Int, 1 :: Int)
                            , (1, 2)
                            , (2, 3)
                            , (3, 4)
                            , (4, 5)
                            , (5, 6)
                            , (6, 7)
                            , (7, 8)
                            , (8, 9)
                            , (9, 10)
                            , (10, 11)
                            ]
            nextIndex paginatedMap 0 `shouldBe` Just 10
            previousIndex paginatedMap 10 `shouldBe` Just 0
            nextIndex paginatedMap 10 `shouldBe` Nothing
            previousIndex paginatedMap 0 `shouldBe` Nothing
        it "works on sparse indices" $ do
            let paginatedMap =
                    mkStrictMapPaginate 3
                        $ Map.fromList
                            [ (2 :: Int, 1 :: Int)
                            , (3, 3)
                            , (5, 5)
                            , (9, 7)
                            , (11, 9)
                            , (19, 11)
                            , (20, 13)
                            ]
            minIndex paginatedMap `shouldBe` Just 2
            pageAtIndex paginatedMap 2
                `shouldBe` Just
                    (3, Map.fromList [(2, 1), (3, 3), (5, 5)])
            nextIndex paginatedMap 2 `shouldBe` Just 9
            previousIndex paginatedMap 9 `shouldBe` Just 2
            pageAtIndex paginatedMap 9
                `shouldBe` Just (3, Map.fromList [(9, 7), (11, 9), (19, 11)])
            nextIndex paginatedMap 9 `shouldBe` Just 20
            previousIndex paginatedMap 20 `shouldBe` Just 9
            pageAtIndex paginatedMap 20
                `shouldBe` Just (1, Map.fromList [(20, 13)])
            nextIndex paginatedMap 20 `shouldBe` Nothing
