{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( someFunc,
  )
where

import Data.Text
import Lucid
import Lucid.Generate hiding (Html)
import Lucid.Supplemental
import Lucid.Servant
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Server
import Lucid.Combinators (html5S)
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics

someFunc :: IO ()
someFunc = run 8080 app

data Payload = Payload
    { input :: Text
    } deriving (Generic, FromJSON)

app :: Application
app = serve (Proxy :: Proxy API) server

type API = Get '[HTML] Index :<|> ("translate" :> ReqBody '[JSON] Payload :> Post '[HTML] Text)

server :: Server API
server = returnIndex :<|> returnLucid
  where
    returnIndex :: Handler Index
    returnIndex = pure Index

    returnLucid :: Payload -> Handler Text
    returnLucid (Payload html) = do
        liftIO $ print html
        pure . pack $ lucidFromHtml html5S (Options False True 4) "result" $ unpack html

data Index = Index

instance ToHtml Index where
  toHtml _ = do
    doctype_

    html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        title_ "HTML -> Lucid"

        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
        script_ [src_ "https://unpkg.com/htmx.org@1.6.1"] ("" :: Html ())
        script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] ("" :: Html ())

      body_ $ do
        h1_ [class_ "text-2xl m-10 font-semibold"] "HTML -> Lucid"
        div_ [class_ "m-10 flex flex-col lg:flex-row"] $ do
          div_ [class_ "flex flex-col grow"] $ do
            h1_ [class_ "text-xl"] "HTML"
            textarea_ [class_ "mt-5 rounded-md p-2 border border-2 border-gray-500 h-screen", wrap_ "off", id_ "input", name_ "input"] "Put HTML here"
          div_ [class_ "flex flex-col my-5 lg:mx-10 self-center"] $ do
              button_
                [ class_ "text-semibold text-white bg-blue-500 rounded-md shadow-md py-2 px-4"
                , hxPost_ "/translate"
                , hxInclude_ "#input"
                , hxTarget_ "#output"
                , hxTrigger_ "click"
                , hxSwap_ "innerHTML"
                , hxExt_ "json-enc"
                ]
                "Translate"
          div_ [class_ "flex flex-col grow"] $ do
            h1_ [class_ "text-xl"] "Lucid"
            textarea_ [class_ "mt-5 rounded-md p-2 border border-2 border-gray-500 h-screen", wrap_ "off", id_ "output"] ""

  toHtmlRaw = toHtml
