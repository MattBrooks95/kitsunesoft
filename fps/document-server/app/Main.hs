{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T (
    Text
    , concat
    )

import qualified Data.Map as M

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/sheet/#T.Text SheetR GET
/sheets SheetsR GET
|]

instance Yesod HelloWorld

dummySheets :: M.Map T.Text T.Text
dummySheets = M.fromList [
    ("sheet1", "a,b,c,d,e,f")
    , ("sheet2", "g,h,i,j,k,l,m,n")
    , ("sheet3", "o,p,q,r,s,t,u,v,w,x,y,z")
    ]

getSheetsR :: Handler Value
getSheetsR = pure (toJSON (M.keys dummySheets))

getSheetR :: T.Text -> Handler T.Text
getSheetR sheetName = case M.lookup sheetName dummySheets of
    Nothing -> pure $ T.concat ["couldn't find sheet with name", sheetName]
    Just sheetContents -> pure sheetContents

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
